//! Encoding of the coherent generational exit snapshot.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum GcLogFormat {
    #[default]
    Text,
    Json,
}
impl std::str::FromStr for GcLogFormat {
    type Err = &'static str;
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "text" => Ok(Self::Text),
            "json" => Ok(Self::Json),
            _ => Err("--gc-log-format must be text or json"),
        }
    }
}
impl GcLogFormat {
    pub(crate) fn header(self) -> &'static [u8] {
        match self {
            Self::Text => b"gc[generational]:",
            Self::Json => {
                b"{\"schema_version\":1,\"collector\":\"generational\",\"event\":\"exit_snapshot\""
            }
        }
    }
    pub(crate) fn footer(self) -> &'static [u8] {
        match self {
            Self::Text => b"\n",
            Self::Json => b"}\n",
        }
    }
    pub(crate) fn field(self, text: &[u8]) -> Vec<u8> {
        match self {
            Self::Text => text.to_vec(),
            Self::Json => {
                // Names are compiler-owned ASCII identifiers, never user input.
                let name = text
                    .strip_prefix(b" ")
                    .and_then(|v| v.strip_suffix(b"="))
                    .expect("GC text field has a name=value prefix");
                assert!(name.iter().all(|b| b.is_ascii_alphanumeric() || *b == b'_'));
                let mut field = b",\"".to_vec();
                field.extend_from_slice(name);
                field.extend_from_slice(b"\":");
                field
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn log_format_rejects_unknown_modes_and_legacy_compiler_configuration() {
        for bad in ["", "Json", "yaml"] {
            assert!(bad.parse::<GcLogFormat>().is_err());
        }
        for format in [GcLogFormat::Text, GcLogFormat::Json] {
            let config = crate::NativeCompilerConfig {
                gc_log_format: Some(format),
                ..Default::default()
            };
            let error = crate::compile_source_for_target("log.kl", "1", config).unwrap_err();
            assert!(format!("{error:?}").contains("--gc-log-format requires --gc=generational"));
        }
    }
}
