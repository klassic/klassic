//! Static function-module bundles. The native inliner's namespace walker is
//! reused after lexical lifting; selectors and source provenance remain exact.
use crate::{NativeCompileError, generational_nested};
use klassic_rewrite::rewrite_expression;
use klassic_span::{Diagnostic, SourceFile, Span};
use klassic_syntax::{Expr, parse_source};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
pub struct GenerationalModuleSource {
    pub path: String,
    pub name: String,
    pub text: String,
}
impl GenerationalModuleSource {
    pub fn new(path: impl Into<String>, name: impl Into<String>, text: impl Into<String>) -> Self {
        Self {
            path: path.into(),
            name: name.into(),
            text: text.into(),
        }
    }
}
#[derive(Clone)]
struct Segment {
    base: usize,
    source: SourceFile,
}
#[derive(Clone)]
pub(crate) struct Sources {
    segments: Vec<Segment>,
    symbols: HashMap<String, String>,
}
impl Sources {
    fn new(source: SourceFile) -> Self {
        Self {
            segments: vec![Segment { base: 0, source }],
            symbols: HashMap::new(),
        }
    }
    fn push(&mut self, source: SourceFile) -> usize {
        let previous = self.segments.last().unwrap();
        let base = previous.base + previous.source.text().len() + 1;
        self.segments.push(Segment { base, source });
        base
    }
    fn locate(&self, span: Span) -> (&SourceFile, Span) {
        let segment = self
            .segments
            .iter()
            .rev()
            .find(|s| span.start >= s.base && span.start <= s.base + s.source.text().len())
            .unwrap_or(&self.segments[0]);
        (
            &segment.source,
            Span::new(
                span.start.saturating_sub(segment.base),
                span.end.saturating_sub(segment.base),
            ),
        )
    }
    pub(crate) fn prefix(&self, span: Span, name: Option<String>) -> String {
        let (source, span) = self.locate(span);
        let mut diagnostic = Diagnostic::runtime(span, "");
        diagnostic.call_name = name;
        diagnostic.render(source)
    }
    pub(crate) fn error(&self, mut diagnostic: Diagnostic) -> NativeCompileError {
        let mut symbols = self.symbols.iter().collect::<Vec<_>>();
        symbols.sort_by_key(|(symbol, _)| std::cmp::Reverse(symbol.len()));
        let input = &diagnostic.message;
        let mut message = String::new();
        let mut offset = 0;
        while offset < input.len() {
            if let Some((symbol, original)) = symbols
                .iter()
                .find(|(symbol, _)| input[offset..].starts_with(symbol.as_str()))
            {
                message.push_str(original);
                offset += symbol.len();
            } else {
                let ch = input[offset..].chars().next().unwrap();
                message.push(ch);
                offset += ch.len_utf8();
            }
        }
        diagnostic.message = message;
        let span = diagnostic.span.unwrap_or(Span::new(0, 0));
        let (source, span) = self.locate(span);
        diagnostic.span = Some(span);
        NativeCompileError::with_view(source.clone(), None, diagnostic)
    }
}
pub(crate) struct Program {
    pub expr: Expr,
    pub names: HashMap<String, String>,
    pub sources: Sources,
}
#[derive(Clone)]
struct Import {
    path: String,
    raw: String,
    alias: Option<String>,
    members: Option<Vec<String>>,
    excludes: Vec<String>,
    span: Span,
}
struct Unit {
    path: Option<String>,
    source: SourceFile,
    expr: Expr,
    imports: Vec<Import>,
    public: Vec<String>,
}
type Exports = HashMap<String, String>;
fn unsupported(span: Span, message: &str) -> Diagnostic {
    Diagnostic::compile(
        span,
        format!("{message} is not supported by canonical module lowering yet"),
    )
}
fn items(expr: &Expr) -> &[Expr] {
    if let Expr::Block { expressions, .. } = expr {
        expressions
    } else {
        std::slice::from_ref(expr)
    }
}
fn walk(
    expr: &mut Expr,
    visitor: &mut impl FnMut(&mut Expr) -> Result<(), Diagnostic>,
) -> Result<(), Diagnostic> {
    match expr {
        Expr::DefDecl { body, .. } | Expr::Unary { expr: body, .. } => walk(body, visitor)?,
        Expr::VarDecl { value, .. } | Expr::Assign { value, .. } => walk(value, visitor)?,
        Expr::Block { expressions, .. } => {
            for child in expressions {
                walk(child, visitor)?;
            }
        }
        Expr::Call {
            callee, arguments, ..
        } => {
            walk(callee, visitor)?;
            for child in arguments {
                walk(child, visitor)?;
            }
        }
        Expr::Binary { lhs, rhs, .. } => {
            walk(lhs, visitor)?;
            walk(rhs, visitor)?;
        }
        Expr::RecordLiteral { fields, .. } => {
            for (_, value) in fields {
                walk(value, visitor)?;
            }
        }
        Expr::FieldAccess { target, .. } => walk(target, visitor)?,
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            walk(condition, visitor)?;
            walk(then_branch, visitor)?;
            if let Some(child) = else_branch {
                walk(child, visitor)?;
            }
        }
        Expr::While {
            condition, body, ..
        } => {
            walk(condition, visitor)?;
            walk(body, visitor)?;
        }
        _ => {}
    }
    visitor(expr)
}
fn relocate(expr: &mut Expr, base: usize) -> Result<(), Diagnostic> {
    walk(expr, &mut |node| {
        let original = node.span();
        let shifted = Span::new(original.start + base, original.end + base);
        match node {
            Expr::DefDecl {
                type_params,
                constraints,
                param_annotations,
                return_annotation,
                ..
            } => {
                if !type_params.is_empty() || !constraints.is_empty() {
                    return Err(unsupported(shifted, "generic module functions"));
                }
                for annotation in param_annotations
                    .iter_mut()
                    .flatten()
                    .chain(return_annotation.iter_mut())
                {
                    annotation.span.start += base;
                    annotation.span.end += base;
                }
            }
            Expr::VarDecl {
                annotation: Some(annotation),
                ..
            } => {
                annotation.span.start += base;
                annotation.span.end += base;
            }
            _ => {}
        }
        match node {
            Expr::Int { span, .. }
            | Expr::Double { span, .. }
            | Expr::Bool { span, .. }
            | Expr::String { span, .. }
            | Expr::Unit { span }
            | Expr::Identifier { span, .. }
            | Expr::ModuleHeader { span, .. }
            | Expr::Import { span, .. }
            | Expr::DefDecl { span, .. }
            | Expr::VarDecl { span, .. }
            | Expr::Assign { span, .. }
            | Expr::Unary { span, .. }
            | Expr::Binary { span, .. }
            | Expr::Call { span, .. }
            | Expr::RecordLiteral { span, .. }
            | Expr::FieldAccess { span, .. }
            | Expr::Block { span, .. }
            | Expr::If { span, .. }
            | Expr::While { span, .. } => *span = shifted,
            _ => return Err(unsupported(shifted, "this module function syntax")),
        }
        Ok(())
    })
}
fn validate(expr: &Expr, path: Option<&str>) -> Result<(Vec<Import>, Vec<String>), Diagnostic> {
    let mut imports = Vec::new();
    let mut public = Vec::new();
    let mut prefix = true;
    let mut header = None;
    for (index, item) in items(expr).iter().enumerate() {
        match item {
            Expr::ModuleHeader { name, span } => {
                if index != 0 || header.is_some() {
                    return Err(unsupported(*span, "a nonleading module header"));
                }
                if path.is_some_and(|expected| expected != name) {
                    return Err(Diagnostic::compile(
                        *span,
                        format!(
                            "module header `{name}` does not match supplied module `{}`",
                            path.unwrap()
                        ),
                    ));
                }
                header = Some(name);
            }
            Expr::Import {
                path,
                alias,
                members,
                excludes,
                span,
            } => {
                if !prefix {
                    return Err(unsupported(
                        *span,
                        "a module import after executable/declaration statements",
                    ));
                }
                imports.push(Import {
                    path: path.clone(),
                    raw: path.clone(),
                    alias: alias.clone(),
                    members: members.clone(),
                    excludes: excludes.clone(),
                    span: *span,
                });
            }
            Expr::VarDecl {
                mutable,
                annotation,
                span,
                ..
            } if path.is_some() => {
                prefix = false;
                if *mutable {
                    return Err(unsupported(*span, "mutable module values"));
                }
                if !annotation.as_ref().is_some_and(|a| {
                    matches!(
                        a.text.trim(),
                        "Int"
                            | "Long"
                            | "Short"
                            | "Byte"
                            | "Float"
                            | "Double"
                            | "Bool"
                            | "Boolean"
                            | "String"
                    )
                }) {
                    return Err(unsupported(
                        *span,
                        "unannotated or unsupported module value types",
                    ));
                }
            }
            Expr::DefDecl { name, .. } => {
                prefix = false;
                public.push(name.clone());
            }
            _ => {
                prefix = false;
            }
        }
    }
    if path.is_some() && header.is_none() {
        return Err(unsupported(
            expr.span(),
            "a module source without its matching header",
        ));
    }
    Ok((imports, public))
}
fn builtin(path: &str) -> bool {
    matches!(
        path,
        "FileInput"
            | "FileOutput"
            | "StandardInput"
            | "Environment"
            | "CommandLine"
            | "Process"
            | "Dir"
            | "Map"
            | "Set"
    )
}
struct Loader {
    registry: HashMap<String, GenerationalModuleSource>,
    state: HashMap<String, bool>,
    sources: Sources,
    ordered: Vec<Unit>,
}
impl Loader {
    fn resolve(&self, path: &str, span: Span) -> Result<String, Diagnostic> {
        if builtin(path) {
            return Err(unsupported(
                span,
                "modules and imports of builtin namespaces",
            ));
        }
        if self.registry.contains_key(path) {
            return Ok(path.to_owned());
        }
        if !path.contains('.') {
            let matches = self
                .registry
                .keys()
                .filter(|key| key.rsplit('.').next() == Some(path))
                .collect::<Vec<_>>();
            if matches.len() == 1 {
                return Ok(matches[0].clone());
            }
        }
        Err(Diagnostic::compile(
            span,
            format!("unknown or ambiguous module `{path}`; supply its function-module source"),
        ))
    }
    fn load(&mut self, path: &str, span: Span) -> Result<(), Diagnostic> {
        if let Some(done) = self.state.get(path) {
            return if *done {
                Ok(())
            } else {
                Err(Diagnostic::compile(
                    span,
                    format!("module import cycle through `{path}`"),
                ))
            };
        }
        self.state.insert(path.to_owned(), false);
        let module = self.registry[path].clone();
        let source = SourceFile::new(module.name, module.text);
        let base = self.sources.push(source.clone());
        let mut expr = parse_source(&source).map_err(|mut d| {
            if let Some(s) = &mut d.span {
                s.start += base;
                s.end += base;
            }
            d
        })?;
        relocate(&mut expr, base)?;
        let expr = rewrite_expression(expr);
        let (mut imports, public) = validate(&expr, Some(path))?;
        for import in &mut imports {
            import.path = self.resolve(&import.path, import.span)?;
            self.load(&import.path, import.span)?;
        }
        self.ordered.push(Unit {
            path: Some(path.to_owned()),
            source,
            expr,
            imports,
            public,
        });
        self.state.insert(path.to_owned(), true);
        Ok(())
    }
}
fn bindings(imports: &[Import], exports: &HashMap<String, Exports>) -> Result<Exports, Diagnostic> {
    let mut bound = Exports::new();
    for import in imports {
        let values = &exports[&import.path];
        if let Some(members) = &import.members {
            for member in members {
                if !import.excludes.contains(member) && !values.contains_key(member) {
                    return Err(Diagnostic::compile(
                        import.span,
                        format!("module `{}` has no member `{member}`", import.raw),
                    ));
                }
            }
        }
        for (member, target) in values {
            if import.excludes.contains(member)
                || import
                    .members
                    .as_ref()
                    .is_some_and(|allowed| !allowed.contains(member))
            {
                continue;
            }
            if let Some(alias) = &import.alias {
                bound.insert(format!("{alias}#{member}"), target.clone());
            }
            if import.members.is_some() || import.alias.is_none() {
                bound.insert(member.clone(), target.clone());
            }
        }
    }
    Ok(bound)
}
fn selected(name: &str, bound: &Exports, exports: &HashMap<String, Exports>) -> Option<String> {
    if let Some(target) = bound.get(name) {
        return Some(target.clone());
    }
    let (path, member) = name.split_once('#')?;
    if let Some(module) = exports.get(path) {
        return module.get(member).cloned();
    }
    if path.contains('.') {
        return None;
    }
    let mut modules = exports
        .iter()
        .filter(|(key, _)| key.rsplit('.').next() == Some(path));
    let (_, first) = modules.next()?;
    if modules.next().is_some() {
        None
    } else {
        first.get(member).cloned()
    }
}
type CallKey = (usize, usize, String);
fn qualified(
    expr: &mut Expr,
    bound: &Exports,
    exports: &HashMap<String, Exports>,
    functions: &HashSet<String>,
    labels: &mut HashMap<CallKey, String>,
) -> Result<(), Diagnostic> {
    // Record selector spelling before visiting its identifier. The evaluator
    // distinguishes `A#f` from `A.f` in the innermost call-name diagnostic.
    fn visit(
        node: &mut Expr,
        bound: &Exports,
        exports: &HashMap<String, Exports>,
        functions: &HashSet<String>,
        labels: &mut HashMap<CallKey, String>,
    ) -> Result<(), Diagnostic> {
        if let Expr::Call { callee, span, .. } = node
            && let Expr::Identifier { name, .. } = callee.as_ref()
            && name.contains('#')
            && let Some(target) = selected(name, bound, exports)
        {
            labels.insert((span.start, span.end, target), name.clone());
        }
        match node {
            Expr::Identifier { name, .. } if name.contains('#') => {
                if let Some(target) = selected(name, bound, exports) {
                    *name = target;
                }
            }
            Expr::FieldAccess {
                target,
                field,
                span,
            } => {
                if let Expr::Identifier { name, .. } = target.as_ref()
                    && !functions.contains(name)
                    && let Some(target) = bound.get(&format!("{name}#{field}"))
                {
                    *node = Expr::Identifier {
                        name: target.clone(),
                        span: *span,
                    };
                }
            }
            _ => {}
        }
        // Pre-order matters for the call-label spelling above.
        match node {
            Expr::DefDecl { body, .. } | Expr::Unary { expr: body, .. } => {
                visit(body, bound, exports, functions, labels)?
            }
            Expr::VarDecl { value, .. } | Expr::Assign { value, .. } => {
                visit(value, bound, exports, functions, labels)?
            }
            Expr::Block { expressions, .. } => {
                for child in expressions {
                    visit(child, bound, exports, functions, labels)?;
                }
            }
            Expr::Call {
                callee, arguments, ..
            } => {
                visit(callee, bound, exports, functions, labels)?;
                for child in arguments {
                    visit(child, bound, exports, functions, labels)?;
                }
            }
            Expr::Binary { lhs, rhs, .. } => {
                visit(lhs, bound, exports, functions, labels)?;
                visit(rhs, bound, exports, functions, labels)?;
            }
            Expr::RecordLiteral { fields, .. } => {
                for (_, child) in fields {
                    visit(child, bound, exports, functions, labels)?;
                }
            }
            Expr::FieldAccess { target, .. } => visit(target, bound, exports, functions, labels)?,
            Expr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                visit(condition, bound, exports, functions, labels)?;
                visit(then_branch, bound, exports, functions, labels)?;
                if let Some(child) = else_branch {
                    visit(child, bound, exports, functions, labels)?;
                }
            }
            Expr::While {
                condition, body, ..
            } => {
                visit(condition, bound, exports, functions, labels)?;
                visit(body, bound, exports, functions, labels)?;
            }
            _ => {}
        }
        Ok(())
    }
    visit(expr, bound, exports, functions, labels)
}

#[allow(clippy::result_large_err)]
pub(crate) fn prepare(
    name: &str,
    text: &str,
    modules: &[GenerationalModuleSource],
) -> Result<Program, NativeCompileError> {
    let source = SourceFile::new(name, text);
    let sources = Sources::new(source.clone());
    let parse = parse_source(&source).map_err(|d| sources.error(d))?;
    let expr = rewrite_expression(parse);
    let (mut imports, public) = validate(&expr, None).map_err(|d| sources.error(d))?;
    let mut registry = HashMap::new();
    for embedded in crate::STDLIB_MODULES {
        registry.insert(
            embedded.path.to_owned(),
            GenerationalModuleSource::new(embedded.path, embedded.diagnostic_name, embedded.source),
        );
    }
    let mut supplied = HashSet::new();
    for module in modules {
        if !supplied.insert(module.path.clone()) || builtin(&module.path) {
            return Err(sources.error(unsupported(
                expr.span(),
                "duplicate or reserved module sources",
            )));
        }
        registry.insert(module.path.clone(), module.clone());
    }
    let mut loader = Loader {
        registry,
        state: HashMap::new(),
        sources,
        ordered: Vec::new(),
    };
    for import in &mut imports {
        import.path = loader
            .resolve(&import.path, import.span)
            .map_err(|d| loader.sources.error(d))?;
        loader
            .load(&import.path, import.span)
            .map_err(|d| loader.sources.error(d))?;
    }
    let mut units = loader.ordered;
    units.push(Unit {
        path: None,
        source,
        expr,
        imports,
        public,
    });
    let refs = units.iter().map(|unit| &unit.expr).collect::<Vec<_>>();
    klassic_types::typecheck_programs_isolated(&refs).map_err(|(_, d)| loader.sources.error(d))?;
    let all_text = units
        .iter()
        .map(|unit| unit.source.text())
        .collect::<Vec<_>>()
        .join("\n");
    let mut exports = HashMap::new();
    let mut names = HashMap::new();
    let mut combined = Vec::new();
    let mut labels = HashMap::new();
    let mut serial = 0;
    let mut alpha = crate::generational_lexical::Alpha::new(&all_text);
    let mut global_values = HashSet::new();
    for unit in units {
        let mut lowered = alpha
            .lower(unit.expr)
            .map_err(|d| loader.sources.error(d))?;
        let functions = lowered.names.keys().cloned().collect::<HashSet<_>>();
        loader.sources.symbols.extend(lowered.bindings.clone());
        let mut bound = bindings(&unit.imports, &exports).map_err(|d| loader.sources.error(d))?;
        for function in &unit.public {
            if bound
                .get(function)
                .is_some_and(|target| global_values.contains(target))
            {
                let span = items(&lowered.expr)
                    .iter()
                    .find_map(|item| {
                        if let Expr::DefDecl { name, span, .. } = item {
                            (name == function).then_some(*span)
                        } else {
                            None
                        }
                    })
                    .unwrap();
                return Err(loader
                    .sources
                    .error(unsupported(span, "a function redefining an imported value")));
            }
        }
        let mut own = HashMap::new();
        if let Some(path) = &unit.path {
            let namespace = loop {
                serial += 1;
                let key = format!("kg_module_{serial}");
                if !all_text.contains(&format!("__mod_{key}_")) {
                    break key;
                }
            };
            for function in &unit.public {
                own.insert(
                    function.clone(),
                    crate::mangled_def_name(&namespace, function),
                );
            }
            for value in &lowered.values {
                let target = crate::mangled_def_name(&namespace, &value.symbol);
                own.insert(value.symbol.clone(), target.clone());
                loader
                    .sources
                    .symbols
                    .insert(target.clone(), value.name.clone());
                global_values.insert(target);
            }
            let mut exported = bound.clone();
            for function in &unit.public {
                exported.insert(function.clone(), own[function].clone());
            }
            for value in &lowered.values {
                exported.insert(value.name.clone(), own[&value.symbol].clone());
            }
            exports.insert(path.clone(), exported);
        }
        let mut dot_locals = functions.clone();
        walk(&mut lowered.expr, &mut |node| {
            match node {
                Expr::DefDecl { params, .. } => dot_locals.extend(params.iter().cloned()),
                Expr::VarDecl { name, .. } => {
                    dot_locals.insert(name.clone());
                }
                _ => {}
            }
            Ok(())
        })
        .map_err(|d| loader.sources.error(d))?;
        qualified(
            &mut lowered.expr,
            &bound,
            &exports,
            &dot_locals,
            &mut labels,
        )
        .map_err(|d| loader.sources.error(d))?;
        bound.retain(|name, _| !name.contains('#'));
        let scope = crate::RenameScope {
            own: own.clone(),
            imported: bound,
            user_aliases: HashMap::new(),
            stdlib_aliases: HashSet::new(),
            inlined: unit.imports.iter().map(|i| i.raw.clone()).collect(),
        };
        for (function, display) in lowered.names {
            names.insert(own.get(&function).unwrap_or(&function).clone(), display);
        }
        let Expr::Block {
            expressions: declarations,
            ..
        } = lowered.expr
        else {
            unreachable!()
        };
        if unit.path.is_some() {
            for mut declaration in declarations {
                if matches!(declaration, Expr::ModuleHeader { .. }) {
                    continue;
                }
                if let Expr::VarDecl { name, .. } = &mut declaration {
                    *name = own[name].clone();
                }
                combined.push(crate::rename_scope_expr(
                    declaration,
                    &scope,
                    &HashSet::new(),
                ));
            }
        } else {
            let expr = Expr::Block {
                expressions: declarations
                    .into_iter()
                    .map(|d| {
                        if let Expr::ModuleHeader { span, .. } = d {
                            Expr::Unit { span }
                        } else {
                            d
                        }
                    })
                    .collect(),
                span: Span::new(0, text.len()),
            };
            let renamed =
                crate::rename_scope_expr(expr, &scope, &unit.public.into_iter().collect());
            let Expr::Block { expressions, .. } = renamed else {
                unreachable!()
            };
            combined.extend(expressions);
        }
    }
    // Specialize only the immutable diagnostic call label, not the collector
    // or function semantics. The cloned body calls the same original children;
    // a recursive bare call therefore resets its name as the evaluator does.
    let definitions = combined
        .iter()
        .filter_map(|e| {
            if let Expr::DefDecl { name, .. } = e {
                Some((name.clone(), e.clone()))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();
    let mut variants = HashMap::new();
    let mut requested = labels
        .iter()
        .filter_map(|((_, _, target), label)| {
            names
                .get(target)
                .filter(|current| *current != label)
                .map(|_| (target.clone(), label.clone()))
        })
        .collect::<Vec<_>>();
    requested.sort();
    requested.dedup();
    let mut additions: HashMap<String, Vec<Expr>> = HashMap::new();
    for (target, label) in requested {
        let variant = loop {
            serial += 1;
            let name = format!("__kg_module_call_{serial}");
            if !all_text.contains(&name) && !names.contains_key(&name) {
                break name;
            }
        };
        let mut clone = definitions[&target].clone();
        let Expr::DefDecl { name, .. } = &mut clone else {
            unreachable!()
        };
        *name = variant.clone();
        names.insert(variant.clone(), label.clone());
        variants.insert((target.clone(), label), variant);
        additions.entry(target).or_default().push(clone);
    }
    let mut ordered = Vec::new();
    for item in combined {
        let extra = if let Expr::DefDecl { name, .. } = &item {
            additions.remove(name).unwrap_or_default()
        } else {
            Vec::new()
        };
        ordered.push(item);
        ordered.extend(extra);
    }
    let start = ordered.first().map_or(0, |item| item.span().start);
    let end = ordered
        .iter()
        .map(|item| item.span().end)
        .max()
        .unwrap_or(start);
    let mut expr = Expr::Block {
        expressions: ordered,
        span: Span::new(start, end),
    };
    walk(&mut expr, &mut |node| {
        if let Expr::Call { callee, span, .. } = node
            && let Expr::Identifier { name, .. } = callee.as_mut()
            && let Some(label) = labels.get(&(span.start, span.end, name.clone()))
            && let Some(variant) = variants.get(&(name.clone(), label.clone()))
        {
            *name = variant.clone();
        }
        Ok(())
    })
    .map_err(|d| loader.sources.error(d))?;
    // One package-wide pass transports immutable module environments through
    // all cross-module and diagnostic-variant calls using actual rooted args.
    loader.sources.symbols.extend(names.clone());
    let lowered = generational_nested::lower(expr, &loader.sources.segments[0].source)
        .map_err(|d| loader.sources.error(d))?;
    for (symbol, original) in &lowered.binding_names {
        let display = loader
            .sources
            .symbols
            .get(original)
            .cloned()
            .unwrap_or_else(|| original.clone());
        loader.sources.symbols.insert(symbol.clone(), display);
    }
    let names: HashMap<String, String> = lowered
        .function_names
        .into_iter()
        .map(|(target, original)| {
            let display = names.get(&original).cloned().unwrap_or(original);
            (target, display)
        })
        .collect();
    let expr = lowered.expr;
    loader.sources.symbols.extend(names.clone());
    klassic_types::typecheck_program_isolated(&expr).map_err(|d| loader.sources.error(d))?;
    Ok(Program {
        expr,
        names,
        sources: loader.sources,
    })
}
