//! Utilities for working [u8]

/// Conversion routine for Debug printing & C code generation.
pub fn as_bstr(input: &[u8]) -> String {
    let mut result = String::new();

    for e in input {
        match char::from_u32(*e as u32) {
            Some('\n') => result += "\\n",
            Some('\r') => result += "\\r",
            Some('\t') => result += "\\t",
            Some('"') => result += "\\\"",
            Some('\\') => result += "\\",
            Some(c)
                if c.is_ascii_alphanumeric()
                    || c.is_ascii_whitespace()
                    || c.is_ascii_punctuation() =>
            {
                result.push(c)
            }
            Some(_) | None => result += format!("\\x{e:02x}").as_str(),
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::as_bstr;

    #[test]
    fn test_as_bstr() {
        assert_eq!(b"\n\r".len(), 2);
        assert_eq!(as_bstr(b"\n\r").len(), 4);
        assert_eq!(as_bstr(b"a\" '"), "a\\\" '");
        assert_eq!(as_bstr(b"abc def"), "abc def");
        assert_eq!(as_bstr(b">>[+-,.]<<!"), ">>[+-,.]<<!");
        assert_eq!(as_bstr(b"ab\x03c"), "ab\\x03c");
        assert_eq!(as_bstr(b"\\a\t"), "\\a\\t");
        assert_eq!(as_bstr(b"ab\r\nc"), "ab\\r\\nc");
    }
}
