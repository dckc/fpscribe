#![crate_type = "lib"]

pub mod io {
    pub use std::io::{IoResult, IoError};
    pub use std::io::IoErrorKind;
    pub use std::io::{PermissionDenied,
                      MismatchedFileTypeForOperation
                      // etc.
    };
}
