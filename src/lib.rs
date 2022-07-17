//! Pattern matching based on regular expressions (but with a bit more power).
mod engine;
pub mod program;
mod prune;

pub use engine::Engine;
pub use program::{Instr, Program};

#[cfg(test)]
mod tests;
