//! Filesystem surface used by the evaluator and the future runtime
//! call path.
//!
//! Separated from [`crate::system::SystemInterface`] so an embedding can
//! provide a fully sandboxed system implementation that simply omits the
//! filesystem trait.

use crate::error::RuntimeError;

pub trait FileSystemInterface {
    fn read_to_string(&self, path: &str) -> Result<String, RuntimeError>;
    fn read_lines(&self, path: &str) -> Result<Vec<String>, RuntimeError>;
    fn write(&self, path: &str, text: &str) -> Result<(), RuntimeError>;
    fn append(&self, path: &str, text: &str) -> Result<(), RuntimeError>;
    fn exists(&self, path: &str) -> bool;
    fn delete(&self, path: &str) -> Result<(), RuntimeError>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;
    use std::collections::HashMap;

    struct InMemoryFs {
        files: RefCell<HashMap<String, String>>,
    }

    impl FileSystemInterface for InMemoryFs {
        fn read_to_string(&self, path: &str) -> Result<String, RuntimeError> {
            self.files
                .borrow()
                .get(path)
                .cloned()
                .ok_or_else(|| RuntimeError::io(format!("file not found: {path}")))
        }
        fn read_lines(&self, path: &str) -> Result<Vec<String>, RuntimeError> {
            Ok(self
                .read_to_string(path)?
                .lines()
                .map(|line| line.to_string())
                .collect())
        }
        fn write(&self, path: &str, text: &str) -> Result<(), RuntimeError> {
            self.files
                .borrow_mut()
                .insert(path.to_string(), text.to_string());
            Ok(())
        }
        fn append(&self, path: &str, text: &str) -> Result<(), RuntimeError> {
            let mut files = self.files.borrow_mut();
            let entry = files.entry(path.to_string()).or_default();
            entry.push_str(text);
            Ok(())
        }
        fn exists(&self, path: &str) -> bool {
            self.files.borrow().contains_key(path)
        }
        fn delete(&self, path: &str) -> Result<(), RuntimeError> {
            self.files
                .borrow_mut()
                .remove(path)
                .map(|_| ())
                .ok_or_else(|| RuntimeError::io(format!("file not found: {path}")))
        }
    }

    #[test]
    fn in_memory_fs_round_trips_through_dyn_trait() {
        let fs: Box<dyn FileSystemInterface> = Box::new(InMemoryFs {
            files: RefCell::new(HashMap::new()),
        });
        fs.write("/tmp/hello", "hi\nworld\n").unwrap();
        assert!(fs.exists("/tmp/hello"));
        assert_eq!(fs.read_to_string("/tmp/hello").unwrap(), "hi\nworld\n");
        assert_eq!(fs.read_lines("/tmp/hello").unwrap(), vec!["hi", "world"]);
        fs.append("/tmp/hello", "tail\n").unwrap();
        assert_eq!(
            fs.read_to_string("/tmp/hello").unwrap(),
            "hi\nworld\ntail\n"
        );
        fs.delete("/tmp/hello").unwrap();
        assert!(!fs.exists("/tmp/hello"));
        let missing = fs.read_to_string("/tmp/missing").unwrap_err();
        assert_eq!(missing.kind, crate::error::RuntimeErrorKind::Io);
    }
}
