use std::{
    collections::VecDeque,
    fmt,
    hash::Hash,
    ops::{Deref, DerefMut, Index, IndexMut},
};

use indexmap::IndexSet;

use crate::engine::Engine;

/// A program for the VM
#[derive(derivative::Derivative)]
#[derivative(Debug(bound = "E::Consume: fmt::Debug, E::Peek: fmt::Debug"))]
#[derivative(Default(bound = ""))]
pub struct Program<E: Engine> {
    /// List of instructions. `InstrPtr`s are indexed into this vector
    pub(crate) prog: Vec<Instr<E>>,
}

impl<E: Engine> Program<E> {
    pub fn new() -> Program<E> {
        Default::default()
    }

    /// Creates a new `Program` which can match at any location, not just the start of the string.
    pub fn floating_start() -> Program<E> {
        let mut this = Program::new();
        match this.zero_or_more(Instr::Any, false) {
            Ok(_) => {}
            Err(e) => match e {},
        };
        this
    }

    /// Append the given pattern to the program.
    pub fn concatenate<P>(&mut self, pattern: P) -> Result<&mut Self, P::Error>
    where
        P: Pattern<E>,
    {
        pattern.write_program(self).map(|()| self)
    }

    /// A builder for a program which will match from a set of sub-patterns. Earlier sub-patterns
    /// will be preferentially matched.
    pub fn alternates(&mut self) -> Alternates<'_, E> {
        // skip over the jump instruction
        let start_of_pattern = self.len() + 2;
        self.push(Instr::Jump(start_of_pattern));
        let jump_instr = self.len();
        // to be filled in later with a jump to after the end of the alternates
        self.push(Instr::Reject);
        Alternates {
            program: self,
            jump_instr,
            split: None,
        }
    }

    /// Matches the pattern zero or one times. If `greedy` is set to `true`, prefer matching the
    /// pattern once; if set to `false`, prefer matching it zero times.
    pub fn zero_or_one<T>(&mut self, pattern: T, greedy: bool) -> Result<&mut Self, T::Error>
    where
        T: Pattern<E>,
    {
        let split = self.len();
        // to be filled in later
        self.push(Instr::Reject);
        pattern.write_program(self)?;
        let here = self.len();
        if greedy {
            // prefer to match
            self[split] = Instr::Split(here);
        } else {
            // prefer to skip
            self[split] = Instr::JSplit(here);
        }
        Ok(self)
    }

    /// Matches the pattern zero or more times. If `greedy` is set to `true`, prefer matching the
    /// pattern as many times as possible; if set to `false`, prefer matching it as few times as
    /// possible.
    pub fn zero_or_more<T>(&mut self, pattern: T, greedy: bool) -> Result<&mut Self, T::Error>
    where
        T: Pattern<E>,
    {
        let split = self.len();
        // to be filled in later
        self.push(Instr::Reject);
        pattern.write_program(self)?;
        // jump to start of loop
        self.push(Instr::Jump(split));
        let here = self.len();
        if greedy {
            // prefer to match
            self[split] = Instr::Split(here);
        } else {
            // prefer to skip
            self[split] = Instr::JSplit(here);
        }
        Ok(self)
    }

    /// Matches the pattern one or more times. If `greedy` is set to `true`, prefer matching the
    /// pattern as many times as possible; if set to `false`, prefer matching it as few times as
    /// possible.
    pub fn one_or_more<T>(&mut self, pattern: T, greedy: bool) -> Result<&mut Self, T::Error>
    where
        T: Pattern<E>,
    {
        let start = self.len();
        pattern.write_program(self)?;
        // jump to start of loop
        if greedy {
            // prefer to loop
            self.push(Instr::JSplit(start));
        } else {
            // prefer to continue
            self.push(Instr::Split(start));
        }
        Ok(self)
    }

    /// Pushes the given [`Peek`](Instr::Peek) instruction to the program.
    pub fn peek(&mut self, args: E::Peek) {
        self.push(Instr::Peek(args));
    }

    /// Pushes the given [`Consume`](Instr::Consume) instruction to the program.
    pub fn consume(&mut self, args: E::Consume) {
        self.push(Instr::Consume(args));
    }

    /// Executes the program. Returns a vector of matches found. For each match, the state of the
    /// engine is returned.
    pub fn exec<I>(&self, initial_state: E, input: I) -> Vec<E>
    where
        I: IntoIterator<Item = E::Token>,
    {
        let mut states = vec![initial_state];
        self.exec_multiple(&mut states, input);
        states
    }

    pub fn exec_multiple<I>(&self, states: &mut Vec<E>, input: I)
    where
        I: IntoIterator<Item = E::Token>,
    {
        let input = input
            .into_iter()
            .map(Some)
            .chain(std::iter::once(None))
            .enumerate();

        // start initial threads at start instruction
        let mut executor = EvaluationState::new(self, states.drain(..));

        // iterate over tokens of input string
        for (i, tok_i) in input {
            // iterate over active threads, draining the list so we can reuse it without
            // reallocating
            executor.step(i, tok_i.as_ref());
        }

        states.extend(executor.finish())
    }
}

impl<E: Engine> fmt::Display for Program<E>
where
    E::Peek: fmt::Debug,
    E::Consume: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let width = self.prog.len().to_string().len();
        for (i, instr) in self.prog.iter().enumerate() {
            writeln!(f, "{i:width$}: {instr:?}")?;
        }
        Ok(())
    }
}

impl<E: Engine> Deref for Program<E> {
    type Target = Vec<Instr<E>>;

    fn deref(&self) -> &Self::Target {
        &self.prog
    }
}

impl<E: Engine> DerefMut for Program<E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.prog
    }
}

impl<E: Engine> Index<InstrPtr> for Program<E> {
    type Output = Instr<E>;

    fn index(&self, idx: InstrPtr) -> &Instr<E> {
        // allow "one-past-the-end" jumps, resulting in a successful match
        if idx == self.prog.len() {
            &Instr::Match
        } else {
            self.prog.index(idx)
        }
    }
}

impl<E: Engine> IndexMut<InstrPtr> for Program<E> {
    fn index_mut(&mut self, idx: InstrPtr) -> &mut Instr<E> {
        // do not allow "one-past-the-end", since that wouldn't make sense for a mutable index
        self.prog.index_mut(idx)
    }
}

/// Created by [`Program::alternates`].
pub struct Alternates<'p, E: Engine> {
    program: &'p mut Program<E>,
    /// Location of the instruction to be filled in with a jump to the end of the list of
    /// alternates.
    jump_instr: InstrPtr,
    /// Location of the previous `Split` instruction.
    split: Option<InstrPtr>,
}

impl<E: Engine> Alternates<'_, E> {
    /// Add an alternate sub-pattern.
    pub fn add<T>(&mut self, pattern: T) -> Result<&mut Self, T::Error>
    where
        T: Pattern<E>,
    {
        let here = self.program.len();
        if let Some(split) = self.split {
            self.program[split] = Instr::Split(here);
        }
        self.split = Some(here);
        // to be filled in later with a split to the next alternate
        self.program.push(Instr::Reject);
        pattern.write_program(self.program)?;
        self.program.push(Instr::Jump(self.jump_instr));
        Ok(self)
    }

    /// Add an alternate sub-pattern and finish the list of alternates. This produces a slightly
    /// more concise program than calling [`self.add(pattern)`](Self::add) followed by
    /// [`self.finish()`](Self::finish)
    pub fn add_finish<T>(&mut self, pattern: T) -> Result<&mut Program<E>, T::Error>
    where
        T: Pattern<E>,
    {
        let here = self.program.len();
        if let Some(split) = self.split {
            self.program[split] = Instr::Split(here);
            pattern.write_program(self.program)?;
            // don't need to do the indirect jump for this alternate
            // still need to update the target of other jumps
            let here = self.program.len();
            self.program[self.jump_instr] = Instr::Jump(here);
        } else {
            // this is the only alternate; this should be equivalent to just calling
            // `write_program` without any of the `alternates` business.
            debug_assert_eq!(self.program.len(), self.jump_instr + 1);
            // jump to end of alternates
            self.program.pop();
            // jump to start of alternates
            self.program.pop();
            pattern.write_program(self.program)?;
        }
        Ok(self.program)
    }

    pub fn finish(&mut self) -> &mut Program<E> {
        if let Some(split) = self.split {
            // change the split to a no-op
            self.program[split] = Instr::Jump(split + 1);
            // update the jump instruction to point here
            let here = self.program.len();
            self.program[self.jump_instr] = Instr::Jump(here);
        } else {
            // there were no alternates provided; replace the whole thing with a single `Reject`
            debug_assert_eq!(self.program.len(), self.jump_instr + 1);
            // jump to end of alternates
            self.program.pop();
            // jump to start of alternates
            self.program.pop();
            self.program.push(Instr::Reject);
        }
        self.program
    }
}

pub struct EvaluationState<'p, E: Engine> {
    program: &'p Program<E>,
    threads: ThreadList<E>,
}

impl<'p, E: Engine> EvaluationState<'p, E> {
    pub fn new(program: &'p Program<E>, initial_states: impl IntoIterator<Item = E>) -> Self {
        Self {
            program,
            threads: ThreadList::new(initial_states),
        }
    }

    pub fn step(&mut self, index: usize, token: Option<&E::Token>) {
        while let Some(thread) = self.threads.pop() {
            self.threads.consume_(index, token, self.program, thread);
        }
        self.threads.reset();
    }

    pub fn finish(self) -> impl Iterator<Item = E> + use<'p, E> {
        self.threads.matches.into_iter()
    }
}

pub trait Pattern<E: Engine> {
    type Error;

    fn write_program(self, program: &mut Program<E>) -> Result<(), Self::Error>;
}

impl<E: Engine> Pattern<E> for Instr<E> {
    type Error = std::convert::Infallible;

    fn write_program(self, program: &mut Program<E>) -> Result<(), Self::Error> {
        program.push(self);
        Ok(())
    }
}

impl<E, I, P> Pattern<E> for I
where
    E: Engine,
    I: IntoIterator<Item = P>,
    P: Pattern<E>,
{
    type Error = P::Error;

    fn write_program(self, program: &mut Program<E>) -> Result<(), Self::Error> {
        self.into_iter()
            .try_fold(program, Program::concatenate)
            .map(|_| ())
    }
}

/// Type for indexing into a program
pub type InstrPtr = usize;

/// A single instruction
#[derive(derivative::Derivative)]
#[derivative(Debug(bound = "E::Consume: fmt::Debug, E::Peek: fmt::Debug"))]
pub enum Instr<E: Engine> {
    /// Splits into two states, preferring not to jump. Used to implement alternations and
    /// quantifiers
    Split(InstrPtr),
    /// Splits into two states, preferring to jump. Used to implement alternations and quantifiers.
    JSplit(InstrPtr),
    /// Jumps to a new point in the program.
    Jump(InstrPtr),
    /// Consumes a token.
    Any,
    /// Consumes a token. The engine determines whether it matches.
    Consume(E::Consume),
    /// Peeks at the next token without consuming it. The engine determines whether it matches.
    Peek(E::Peek),
    /// Reject a potential match. Can be used after a Map when fallthrough should fail.
    Reject,
    /// The end of a match.
    Match,
}

/// A thread, consisting of an `InstrPtr` to the current instruction, and a vector of all saved
/// positions
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Thread<E> {
    /// Pointer to current instruction, or `None` if the thread is complete
    pc: InstrPtr,
    /// Implementation-specific state
    engine: E,
}

impl<E> Thread<E> {
    /// Create a new `Thread` by advancing the instruction pointer
    fn next(self) -> Self {
        Self {
            pc: self.pc + 1,
            ..self
        }
    }

    /// Create a new `Thread` by advancing the instruction pointer to the specified position
    fn with_pc(self, pc: InstrPtr) -> Self {
        Self { pc, ..self }
    }
}

/// A list of threads
#[derive(Debug)]
struct ThreadList<E> {
    threads: IndexSet<Thread<E>>,
    current: VecDeque<Thread<E>>,
    count: usize,
    matches: IndexSet<E>,
}

impl<E: Hash + Eq> ThreadList<E> {
    fn new(states: impl IntoIterator<Item = E>) -> Self {
        let current: VecDeque<_> = states
            .into_iter()
            .map(|engine| Thread { pc: 0, engine })
            .collect();
        ThreadList {
            threads: IndexSet::new(),
            count: current.len(),
            current,
            matches: IndexSet::new(),
        }
    }

    fn pop(&mut self) -> Option<Thread<E>> {
        if self.count > 0 {
            self.count -= 1;
            self.current.pop_front()
        } else {
            None
        }
    }

    fn reset(&mut self) {
        self.threads.clear();
        self.count = self.current.len();
    }

    fn consume_(&mut self, i: usize, token: Option<&E::Token>, prog: &Program<E>, mut th: Thread<E>)
    where
        E: Engine,
    {
        // prune this thread if necessary
        if !self.threads.insert(th.clone()) {
            return;
        }

        match prog[th.pc] {
            Instr::Split(split) => {
                // branch with no jump is higher priority
                self.consume_(i, token, prog, th.clone().next());
                self.consume_(i, token, prog, th.with_pc(split));
            }
            Instr::JSplit(split) => {
                // branch with jump is higher priority
                self.consume_(i, token, prog, th.clone().with_pc(split));
                self.consume_(i, token, prog, th.next());
            }
            Instr::Jump(jump) => {
                // jump to specified pc
                self.consume_(i, token, prog, th.with_pc(jump));
            }
            Instr::Peek(ref args) => {
                // check if the engine matches here
                if th.engine.peek(args, i, token) {
                    // and recursively add next instruction
                    self.consume_(i, token, prog, th.next());
                }
            }
            Instr::Any => {
                if let Some(token) = token
                    && th.engine.any(i, token)
                {
                    self.current.push_back(th.next());
                }
            }
            Instr::Consume(ref args) => {
                if let Some(token) = token
                    && th.engine.consume(args, i, token)
                {
                    self.current.push_back(th.next());
                }
            }
            Instr::Match => {
                self.matches.insert(th.engine);
            }
            // Reject match
            Instr::Reject => {}
        }
    }
}

impl<E> IntoIterator for ThreadList<E> {
    type Item = Thread<E>;
    type IntoIter = ::indexmap::set::IntoIter<Thread<E>>;

    fn into_iter(self) -> Self::IntoIter {
        self.threads.into_iter()
    }
}
