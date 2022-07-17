use std::fmt;
use std::mem;
use std::ops::{Deref, DerefMut, Index, IndexMut};

use crate::{engine::Engine, prune::PruneList};

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
        let mut input = input.into_iter().enumerate().peekable();

        let mut curr = ThreadList::new(states.len());
        let mut next = ThreadList::new(states.len());

        let mut prune_list = PruneList::new(self.prog.len());

        // start initial thread at start instruction
        let first_tok = input.peek().map(|(_i, tok)| tok);
        for state in states.drain(..) {
            curr.add_thread(0, 0, first_tok, self, &mut prune_list, state);
        }

        let matches = states;

        // iterate over tokens of input string
        while let Some((i, tok_i)) = input.next() {
            // iterate over active threads, draining the list so we can reuse it without
            // reallocating
            for mut th in &mut curr {
                if let Some(pc) = th.pc {
                    match &self[pc] {
                        Instr::Any => {
                            next.add_thread(
                                pc + 1,
                                i + 1,
                                input.peek().map(|(_i, tok)| tok),
                                self,
                                &mut prune_list,
                                th.engine,
                            );
                        }
                        Instr::Consume(args) => {
                            if th.engine.consume(args, i, &tok_i) {
                                next.add_thread(
                                    pc + 1,
                                    i + 1,
                                    input.peek().map(|(_i, tok)| tok),
                                    self,
                                    &mut prune_list,
                                    th.engine,
                                );
                            }
                        }
                        // add the saved locations to the final list
                        Instr::Match => next.add_match(th.engine),
                        // These instructions are handled in add_thread, so the current thread should
                        // never point to one of them
                        Instr::Split(_)
                        | Instr::JSplit(_)
                        | Instr::Jump(_)
                        | Instr::Peek(_)
                        | Instr::Reject => {
                            unreachable!();
                        }
                    }
                } else {
                    next.threads.push(th)
                }
            }
            // `next` becomes list of active threads, and `curr` (empty after iteration) can hold
            // the next iteration
            mem::swap(&mut curr, &mut next);
        }

        // now iterate over remaining threads, to check for matches
        for th in &mut curr {
            if let Instr::Match = th.pc.map(|pc| &self[pc]).unwrap_or(&Instr::Match) {
                matches.push(th.engine);
            }
            // anything else is a failed match
        }
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

impl<E, F, Error> Pattern<E> for F
where
    E: Engine,
    F: FnOnce(&mut Program<E>) -> Result<(), Error>,
{
    type Error = Error;

    fn write_program(self, program: &mut Program<E>) -> Result<(), Self::Error> {
        self(program)
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
#[derive(Debug)]
struct Thread<E> {
    /// Pointer to current instruction, or `None` if the thread is complete
    pc: Option<InstrPtr>,
    /// Implementation-specific state
    engine: E,
}

impl<E> Thread<E> {
    /// Create a new `Thread` with the specified instruction pointer and the given state.
    fn new(pc: InstrPtr, engine: E) -> Self {
        Thread {
            pc: Some(pc),
            engine,
        }
    }

    /// Create a new `Thread` with the given state and no instruction pointer.
    fn new_match(engine: E) -> Self {
        Thread { pc: None, engine }
    }
}

/// A list of threads
#[derive(Debug)]
struct ThreadList<E> {
    threads: Vec<Thread<E>>,
}

impl<E> ThreadList<E> {
    /// Create a new `ThreadList` with a specified capacity
    fn new(cap: usize) -> Self {
        ThreadList {
            threads: Vec::with_capacity(cap),
        }
    }

    /// Add a new `Thread` with the specified instruction pointer, and the given list of saved
    /// locations. If `pc` points to a `Jump`, `Split`, `JSplit`, or `Peek` instruction, calls
    /// `add_thread` recursively, so that the active `ThreadList` never contains pointers to those
    /// instructions.
    fn add_thread(
        &mut self,
        pc: InstrPtr,
        in_idx: usize,
        next_tok: Option<&E::Token>,
        prog: &Program<E>,
        prune_list: &mut PruneList,
        mut engine: E,
    ) where
        E: Engine,
    {
        // prune this thread if necessary
        if prune_list.insert(pc, &engine, in_idx) {
            return;
        }

        match prog[pc] {
            Instr::Split(split) => {
                // call `add_thread` recursively
                // branch with no jump is higher priority
                // clone the `engine` so we can use it again in the second branch
                self.add_thread(pc + 1, in_idx, next_tok, prog, prune_list, engine.clone());
                self.add_thread(split, in_idx, next_tok, prog, prune_list, engine);
            }
            Instr::JSplit(split) => {
                // call `add_thread` recursively
                // branch with jump is higher priority
                // clone the `engine` so we can use it again in the second branch
                self.add_thread(split, in_idx, next_tok, prog, prune_list, engine.clone());
                self.add_thread(pc + 1, in_idx, next_tok, prog, prune_list, engine);
            }
            Instr::Jump(jump) => {
                // call `add_thread` recursively
                // jump to specified pc
                self.add_thread(jump, in_idx, next_tok, prog, prune_list, engine);
            }
            Instr::Peek(ref args) => {
                // check if the engine matches here
                if engine.peek(args, in_idx, next_tok) {
                    // and recursively add next instruction
                    self.add_thread(pc + 1, in_idx, next_tok, prog, prune_list, engine);
                }
            }
            Instr::Reject => {} // do nothing, this thread is dead
            Instr::Any | Instr::Consume(_) | Instr::Match => {
                // push a new thread with the given pc
                self.threads.push(Thread::new(pc, engine));
            }
        }
    }

    fn add_match(&mut self, engine: E) {
        self.threads.push(Thread::new_match(engine))
    }
}

impl<'a, E> IntoIterator for &'a mut ThreadList<E> {
    type Item = Thread<E>;
    type IntoIter = ::std::vec::Drain<'a, Thread<E>>;

    fn into_iter(self) -> Self::IntoIter {
        self.threads.drain(..)
    }
}
