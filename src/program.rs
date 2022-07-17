use std::fmt;
use std::mem;
use std::ops::{Deref, DerefMut, Index, IndexMut};

use crate::{engine::Engine, prune::PruneList};

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

/// A program for the VM
#[derive(derivative::Derivative)]
#[derivative(Debug(bound = "E::Consume: fmt::Debug, E::Peek: fmt::Debug"))]
#[derivative(Default(bound = ""))]
pub struct Program<E: Engine> {
    /// List of instructions. `InstrPtr`s are indexed into this vector
    pub prog: Vec<Instr<E>>,
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

impl<E: Engine> Program<E> {
    pub fn new() -> Program<E> {
        Default::default()
    }

    /// Creates a new `Program` which can match at any location, not just the start of the string.
    pub fn floating_start() -> Program<E> {
        Program {
            prog: vec![Instr::JSplit(3), Instr::Any, Instr::Jump(0)],
        }
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
