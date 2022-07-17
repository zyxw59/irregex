use std::hash::Hash;

/// The [`Engine`] trait defines a specific set of matching behaviors.
pub trait Engine: Hash + Clone {
    /// The type of token the engine operates on.
    type Token;
    /// The type for the [`Consume`](crate::program::Instr::Consume) instruction.
    type Consume;
    /// The type for the [`Peek`](crate::program::Instr::Peek) instruction.
    type Peek;

    /// Call the [`Consume`](crate::program::Instr::Consume) instruction.
    fn consume(&mut self, args: &Self::Consume, index: usize, token: &Self::Token) -> bool;

    /// Call the [`Peek`](crate::program::Instr::Peek) instruction.
    fn peek(&mut self, args: &Self::Peek, index: usize, token: Option<&Self::Token>) -> bool;

    /// Call the [`Any`](crate::program::Instr::Any) instruction. This is necessary if the engine
    /// needs to store state for each token.
    fn any(&mut self, _index: usize, _token: &Self::Token) -> bool {
        true
    }
}
