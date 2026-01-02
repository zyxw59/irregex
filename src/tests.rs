use crate::{
    Program, engine,
    program::{Instr, IrregexInput, ThreadList},
};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Engine {
    saves: Vec<Option<usize>>,
    is_whitespace: bool,
}

impl Engine {
    fn new(num_slots: usize) -> Self {
        Engine {
            saves: vec![None; num_slots],
            is_whitespace: true,
        }
    }
}

impl engine::Engine for Engine {
    type Token = char;
    type Consume = char;
    type Peek = Peek;

    fn consume(&mut self, expected: &Self::Consume, index: usize, token: &Self::Token) -> bool {
        eprintln!("{self:?}.consume({expected:?}, {index}, {token:?})");
        self.is_whitespace = token.is_whitespace();
        expected == token
    }

    fn peek(&mut self, args: &Self::Peek, index: usize, token: Option<&Self::Token>) -> bool {
        eprintln!("{self:?}.peek({args:?}, {index}, {token:?})");
        match args {
            Peek::WordBoundary => token.is_none_or(|tok| tok.is_whitespace() ^ self.is_whitespace),
            Peek::Save(slot) => {
                self.saves[*slot] = Some(index);
                true
            }
        }
    }

    fn any(&mut self, index: usize, token: &Self::Token) -> bool {
        eprintln!("{self:?}.any({index}, {token:?})");
        self.is_whitespace = token.is_whitespace();
        true
    }
}

#[derive(Debug)]
pub enum Peek {
    WordBoundary,
    Save(usize),
}

#[test]
fn program() {
    // /(ab?)(b?c)\b/
    let mut program = Program::floating_start();
    // save start of match
    program.peek(Peek::Save(0));
    // save start of first subgroup
    program.peek(Peek::Save(2));
    // a
    program.consume('a');
    // b?
    program.zero_or_one(Instr::Consume('b'), true).unwrap();
    // save end of first subgroup
    program.peek(Peek::Save(3));
    // save start of second subgroup
    program.peek(Peek::Save(4));
    // b?
    program.zero_or_one(Instr::Consume('b'), true).unwrap();
    // c
    program.consume('c');
    // save end of second subgroup
    program.peek(Peek::Save(5));
    // word boundary
    program.peek(Peek::WordBoundary);
    // save end of match
    program.peek(Peek::Save(1));

    println!("{program}");
    let saves = program.exec(Engine::new(6), "ducabc ".chars());
    assert_eq!(
        saves.iter().map(|engine| &engine.saves).collect::<Vec<_>>(),
        &[
            &[Some(3), Some(6), Some(3), Some(5), Some(5), Some(6)],
            &[Some(3), Some(6), Some(3), Some(4), Some(4), Some(6)],
        ]
    );
    let saves = program.exec(Engine::new(6), "ducabc".chars());
    assert_eq!(
        saves.iter().map(|engine| &engine.saves).collect::<Vec<_>>(),
        &[
            &[Some(3), Some(6), Some(3), Some(5), Some(5), Some(6)],
            &[Some(3), Some(6), Some(3), Some(4), Some(4), Some(6)],
        ]
    );
    let saves = program.exec(Engine::new(6), "ducabcd".chars());
    assert!(saves.is_empty());
}

#[test]
fn precedence_of_alternates() {
    // /ab|b/
    let mut program = Program::floating_start();
    program.peek(Peek::Save(0));
    program
        .alternates()
        .add([Instr::Consume('a'), Instr::Consume('b')])
        .unwrap()
        .add_finish(Instr::Consume('b'))
        .unwrap();
    program.peek(Peek::Save(1));
    println!("{program}");
    let saves = program.exec(Engine::new(2), "ab".chars());
    // 'ab' matches first, even tho 'b' has higher precedence, because 'ab' starts earlier
    assert_eq!(
        saves.iter().map(|engine| &engine.saves).collect::<Vec<_>>(),
        &[&[Some(0), Some(2)], &[Some(1), Some(2)]],
    );
}

#[test]
fn pruning() {
    let mut program = Program::new();
    program.extend([
        /*  0 */ Instr::Split(6),
        /*  1 */ Instr::Split(4),
        /*  2 */ Instr::Peek(Peek::Save(0)),
        /*  3 */ Instr::Jump(10),
        /*  4 */ Instr::Peek(Peek::Save(1)),
        /*  5 */ Instr::Jump(10),
        /*  6 */ Instr::Split(9),
        /*  7 */ Instr::Peek(Peek::Save(0)),
        /*  8 */ Instr::Jump(10),
        /*  9 */ Instr::Peek(Peek::Save(1)),
        /* 10 */ Instr::Any,
    ]);
    println!("{program}");
    let states = program.exec(Engine::new(2), "ab".chars());

    assert_eq!(states.len(), 2);
}

#[test]
fn rejection() {
    let mut program = Program::new();
    program.extend([
        /* 0 */ Instr::Split(3),
        /* 1 */ Instr::Peek(Peek::Save(0)),
        /* 2 */ Instr::Reject,
        /* 3 */ Instr::Peek(Peek::Save(1)),
    ]);
    println!("{program}");
    let states = program.exec(Engine::new(2), "ab".chars());
    assert_eq!(
        states
            .iter()
            .map(|engine| &engine.saves)
            .collect::<Vec<_>>(),
        [&[None, Some(0)]],
    );
}

#[test]
fn short_circuit() {
    // /^(ab?)(b?c)\b/
    let mut program = Program::new();
    // save start of match
    program.peek(Peek::Save(0));
    // save start of first subgroup
    program.peek(Peek::Save(2));
    // a
    program.consume('a');
    // b?
    program.zero_or_one(Instr::Consume('b'), true).unwrap();
    // save end of first subgroup
    program.peek(Peek::Save(3));
    // save start of second subgroup
    program.peek(Peek::Save(4));
    // b?
    program.zero_or_one(Instr::Consume('b'), true).unwrap();
    // c
    program.consume('c');
    // save end of second subgroup
    program.peek(Peek::Save(5));
    // word boundary
    program.peek(Peek::WordBoundary);
    // save end of match
    program.peek(Peek::Save(1));

    println!("{program}");
    let mut input = "abc abbcd abbc".chars();
    let mut input = input.irregex_input();

    fn match_one(
        mut input: impl Iterator<Item = (usize, Option<char>)>,
        program: &Program<Engine>,
    ) -> Option<Engine> {
        input
            .try_fold(
                ThreadList::new(program, [Engine::new(6)]),
                |mut threads, (idx, token)| {
                    use std::ops::ControlFlow;
                    threads.step(idx, token.as_ref());
                    if threads.num_matches() > 0 {
                        ControlFlow::Break(threads.into_matches().next())
                    } else if threads.num_live_threads() == 0 {
                        ControlFlow::Break(None)
                    } else {
                        ControlFlow::Continue(threads)
                    }
                },
            )
            .break_value()
            .flatten()
    }

    assert_eq!(
        match_one(&mut input, &program).unwrap().saves,
        [Some(0), Some(3), Some(0), Some(2), Some(2), Some(3)],
    );

    assert!(match_one(&mut input, &program).is_none());
    assert_eq!(input.next(), Some((9, Some(' '))));

    assert_eq!(
        match_one(&mut input, &program).unwrap().saves,
        [Some(10), Some(14), Some(10), Some(12), Some(12), Some(14)],
    );
}
