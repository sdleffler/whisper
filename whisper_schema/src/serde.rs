//! Whisper optionally supports free interop with any Rust type implementing serde's
//! `Serialize` trait.
//!
//! The encoding is done to make it as efficient as possible to serialize and run queries
//! against data in serialized structs.
//!

use ::{
    derive_more::{Display, From, Into},
    failure::Fail,
    std::fmt,
};

pub mod de;
pub mod ser;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "{}", _0)]
    Message(String),

    #[fail(display = "cannot deserialize from terms with unbound variables")]
    TermMustBeGrounded,

    #[fail(display = "deserialize_any only supports constants, i32, u32, and f32")]
    DeserializeAnyNotSupported,

    #[fail(display = "expected a blob")]
    ExpectedBlob,

    #[fail(display = "unexpected end of input")]
    Eof,

    #[fail(display = "invalid input")]
    Invalid,
}

#[derive(Debug, Display, From, Into)]
pub struct SerdeCompatError(pub Error);

impl std::error::Error for SerdeCompatError {
    fn description(&self) -> &str {
        "An error has occurred."
    }
}

impl serde::ser::Error for SerdeCompatError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::Message(msg.to_string()).into()
    }
}

impl serde::de::Error for SerdeCompatError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::Message(msg.to_string()).into()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn bincode_garbage() {
        let seven_bytes: Vec<u8> = vec![0, 1, 2, 3, 4, 5, 6];
        let mut encoded = bincode::serialize(&seven_bytes).unwrap();
        encoded.push(0xFF); // >:3
        let decoded: Vec<u8> = bincode::deserialize(&encoded).unwrap();
        assert_eq!(seven_bytes, decoded);
    }
}
