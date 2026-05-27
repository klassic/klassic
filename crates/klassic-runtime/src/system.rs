//! Host system surface used by the evaluator and the future runtime
//! call path.
//!
//! The trait is intentionally narrow — only the primitives the language
//! currently exposes (stdio, env, process args, exit code, time, sleep).
//! Filesystem operations live in [`crate::fs::FileSystemInterface`] so a
//! sandboxed embedding can drop one without losing the other.

use crate::error::RuntimeError;

pub trait SystemInterface {
    fn read_stdin_all(&mut self) -> Result<String, RuntimeError>;
    fn read_stdin_lines(&mut self) -> Result<Vec<String>, RuntimeError>;
    fn stdout_write_line(&mut self, text: &str) -> Result<(), RuntimeError>;
    fn stderr_write_line(&mut self, text: &str) -> Result<(), RuntimeError>;

    fn env_get(&self, name: &str) -> Option<String>;
    fn env_vars(&self) -> Vec<String>;
    fn args(&self) -> Vec<String>;

    /// Returns the exit code the program would observe. Implementations
    /// that abort the process must not return; implementations that
    /// merely record the request (e.g. test harnesses) return the value.
    fn exit(&mut self, code: i32) -> i32;

    fn now_millis(&self) -> i64;
    fn sleep_millis(&self, millis: i64) -> Result<(), RuntimeError>;
}

#[cfg(test)]
mod tests {
    use super::*;

    struct RecordedSystem {
        stdout: Vec<String>,
        stderr: Vec<String>,
        env: Vec<(String, String)>,
        args: Vec<String>,
        clock: i64,
        last_exit: Option<i32>,
    }

    impl SystemInterface for RecordedSystem {
        fn read_stdin_all(&mut self) -> Result<String, RuntimeError> {
            Ok(String::new())
        }
        fn read_stdin_lines(&mut self) -> Result<Vec<String>, RuntimeError> {
            Ok(Vec::new())
        }
        fn stdout_write_line(&mut self, text: &str) -> Result<(), RuntimeError> {
            self.stdout.push(text.to_string());
            Ok(())
        }
        fn stderr_write_line(&mut self, text: &str) -> Result<(), RuntimeError> {
            self.stderr.push(text.to_string());
            Ok(())
        }
        fn env_get(&self, name: &str) -> Option<String> {
            self.env
                .iter()
                .find(|(n, _)| n == name)
                .map(|(_, v)| v.clone())
        }
        fn env_vars(&self) -> Vec<String> {
            self.env.iter().map(|(n, v)| format!("{n}={v}")).collect()
        }
        fn args(&self) -> Vec<String> {
            self.args.clone()
        }
        fn exit(&mut self, code: i32) -> i32 {
            self.last_exit = Some(code);
            code
        }
        fn now_millis(&self) -> i64 {
            self.clock
        }
        fn sleep_millis(&self, _millis: i64) -> Result<(), RuntimeError> {
            Ok(())
        }
    }

    #[test]
    fn trait_is_object_safe_via_dyn_dispatch() {
        let mut system: Box<dyn SystemInterface> = Box::new(RecordedSystem {
            stdout: Vec::new(),
            stderr: Vec::new(),
            env: vec![("PATH".to_string(), "/usr/bin".to_string())],
            args: vec!["prog".to_string(), "arg1".to_string()],
            clock: 1_700_000_000_000,
            last_exit: None,
        });
        assert_eq!(system.env_get("PATH").as_deref(), Some("/usr/bin"));
        assert_eq!(system.args(), vec!["prog".to_string(), "arg1".to_string()]);
        assert_eq!(system.now_millis(), 1_700_000_000_000);
        system.stdout_write_line("hi").unwrap();
        system.stderr_write_line("err").unwrap();
        assert_eq!(system.read_stdin_all().unwrap(), "");
        assert!(system.read_stdin_lines().unwrap().is_empty());
        assert!(
            system
                .env_vars()
                .iter()
                .any(|entry| entry == "PATH=/usr/bin")
        );
        system.sleep_millis(0).unwrap();
        assert_eq!(system.exit(0), 0);
    }
}
