//! Every Klassic example printed in the Book has to run.
//!
//! The Book is the first thing a reader copies from, so a block that does not
//! run as printed is a defect even when the surrounding prose is right. Four
//! blocks were once missing the `import` line they needed, which nothing
//! caught because nothing ran them.
//!
//! A block whose info string is ```klassic,norun is extracted but not run --
//! for programs that need arguments or stdin to mean anything. Every other
//! block must exit zero under the evaluator.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn klassic_bin() -> &'static str {
    env!("CARGO_BIN_EXE_klassic")
}

fn book_src() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("docs")
        .join("book")
        .join("src")
}

/// Every `.md` under the Book's source directory, in a stable order.
fn markdown_files(root: &Path) -> Vec<PathBuf> {
    let mut found = Vec::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().is_some_and(|ext| ext == "md") {
                found.push(path);
            }
        }
    }
    found.sort();
    found
}

struct Block {
    file: String,
    index: usize,
    run: bool,
    source: String,
}

/// Fenced blocks whose info string starts with `klassic`.
fn klassic_blocks(text: &str, file: &str) -> Vec<Block> {
    let mut blocks = Vec::new();
    let mut lines = text.lines();
    let mut index = 0;
    while let Some(line) = lines.next() {
        let Some(info) = line.strip_prefix("```") else {
            continue;
        };
        let info = info.trim();
        if info != "klassic" && !info.starts_with("klassic,") {
            continue;
        }
        let mut source = String::new();
        for body in lines.by_ref() {
            if body.trim_end() == "```" {
                break;
            }
            source.push_str(body);
            source.push('\n');
        }
        blocks.push(Block {
            file: file.to_string(),
            index,
            run: !info.contains("norun"),
            source,
        });
        index += 1;
    }
    blocks
}

#[test]
fn every_book_example_runs() {
    let root = book_src();
    let mut blocks = Vec::new();
    for path in markdown_files(&root) {
        let text = std::fs::read_to_string(&path).expect("book page should be readable");
        let name = path
            .strip_prefix(&root)
            .unwrap_or(&path)
            .to_string_lossy()
            .to_string();
        blocks.extend(klassic_blocks(&text, &name));
    }

    assert!(
        blocks.len() >= 30,
        "expected the Book to carry Klassic examples, found {}",
        blocks.len()
    );

    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be after the epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("klassic-book-examples-{stamp}"));
    std::fs::create_dir_all(&dir).expect("temp directory should be creatable");

    let mut failures = Vec::new();
    for block in &blocks {
        if !block.run {
            continue;
        }
        let stem = block.file.replace(['/', '\\', '.'], "_");
        let path = dir.join(format!("{stem}_{}.kl", block.index));
        std::fs::write(&path, &block.source).expect("example should be writable");
        let output = Command::new(klassic_bin())
            .arg(&path)
            .current_dir(&dir)
            .output()
            .expect("klassic should run");
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            let detail = if stderr.trim().is_empty() {
                stdout.trim().to_string()
            } else {
                stderr.trim().to_string()
            };
            failures.push(format!(
                "{}#{}: {}",
                block.file,
                block.index,
                detail.lines().next().unwrap_or("(no output)")
            ));
        }
    }

    let _ = std::fs::remove_dir_all(&dir);
    assert!(
        failures.is_empty(),
        "Book examples that do not run as printed:\n{}",
        failures.join("\n")
    );
}
