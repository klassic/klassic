//! A standalone translation unit must not inherit or export a REPL session's
//! user registries. Move them aside and restore them even when checking unwinds.
use super::*;

pub(super) struct UserTypesGuard {
    modules: HashMap<String, ModuleTypeExports>,
    records: HashMap<String, RecordSchema>,
    enums: HashMap<String, EnumSchema>,
    bindings: HashMap<String, StoredType>,
    classes: HashMap<String, TypeClassInfo>,
    instances: Vec<InstanceInfo>,
    extensions: HashMap<(String, String), StoredType>,
}
impl UserTypesGuard {
    pub(super) fn enter() -> Self {
        Self {
            modules: USER_MODULE_TYPES.with(RefCell::take),
            records: USER_RECORD_SCHEMAS.with(RefCell::take),
            enums: USER_ENUM_SCHEMAS.with(RefCell::take),
            bindings: USER_BINDING_TYPES.with(RefCell::take),
            classes: USER_TYPECLASS_INFOS.with(RefCell::take),
            instances: USER_INSTANCE_INFOS.with(RefCell::take),
            extensions: USER_EXTENSION_METHODS.with(RefCell::take),
        }
    }
}
impl Drop for UserTypesGuard {
    fn drop(&mut self) {
        USER_MODULE_TYPES.with(|x| x.replace(std::mem::take(&mut self.modules)));
        USER_RECORD_SCHEMAS.with(|x| x.replace(std::mem::take(&mut self.records)));
        USER_ENUM_SCHEMAS.with(|x| x.replace(std::mem::take(&mut self.enums)));
        USER_BINDING_TYPES.with(|x| x.replace(std::mem::take(&mut self.bindings)));
        USER_TYPECLASS_INFOS.with(|x| x.replace(std::mem::take(&mut self.classes)));
        USER_INSTANCE_INFOS.with(|x| x.replace(std::mem::take(&mut self.instances)));
        USER_EXTENSION_METHODS.with(|x| x.replace(std::mem::take(&mut self.extensions)));
        // ENUM_DECLARATION_ORDER deliberately remains monotonic, including
        // across discarded translation units (the existing schema contract).
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn ast(source: &str) -> Expr {
        klassic_syntax::parse_source(&klassic_span::SourceFile::new("isolated", source)).unwrap()
    }
    fn seed() {
        typecheck_program(&ast("val retained = 7")).unwrap();
    }
    fn preserved() {
        assert_eq!(
            typecheck_program_with_bindings_typed(
                &ast("retained"),
                std::iter::empty::<(String, KnownType)>()
            )
            .unwrap(),
            "Int"
        );
        typecheck_program(&ast("println(true)")).unwrap();
        assert!(typecheck_program(&ast("introduced")).is_err());
    }
    #[test]
    fn successful_standalone_check_restores_repl_state() {
        let _outer = UserTypesGuard::enter();
        seed();
        typecheck_program_isolated(&ast(
            "def println(n: Int): Int = n\nval introduced = 8\nprintln(1)",
        ))
        .unwrap();
        typecheck_program_isolated(&ast("println(true)")).unwrap();
        preserved();
    }
    #[test]
    fn failed_standalone_check_restores_repl_state() {
        let _outer = UserTypesGuard::enter();
        seed();
        assert!(
            typecheck_program_isolated(&ast(
                "def println(n: Int): Int = n\nval introduced = 8\nprintln(false)"
            ))
            .is_err()
        );
        preserved();
    }
    #[test]
    fn panic_during_isolated_check_restores_repl_state() {
        let _outer = UserTypesGuard::enter();
        seed();
        let result = std::panic::catch_unwind(|| {
            let _check = UserTypesGuard::enter();
            typecheck_program(&ast("def println(n: Int): Int = n\nval introduced = 8")).unwrap();
            panic!("intentional checker unwind");
        });
        assert!(result.is_err());
        preserved();
    }
    #[test]
    fn ordered_module_bundle_restores_session_on_success_and_error() {
        let _outer = UserTypesGuard::enter();
        seed();
        let module = ast("module isolated_bundle\ndef introduced(n: Int): Int = n + 1");
        let entry = ast("import isolated_bundle as M\nM#introduced(41)");
        typecheck_programs_isolated(&[&module, &entry]).unwrap();
        preserved();
        assert!(typecheck_program(&ast("import isolated_bundle")).is_err());
        let invalid = ast("import isolated_bundle.{missing}");
        assert_eq!(
            typecheck_programs_isolated(&[&module, &invalid])
                .unwrap_err()
                .0,
            1
        );
        preserved();
        assert!(typecheck_program(&ast("import isolated_bundle")).is_err());
    }
}
