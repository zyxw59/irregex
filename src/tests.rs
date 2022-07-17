use crate::{engine, program, Program};

#[derive(Clone, Debug, Hash)]
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

    fn consume(&mut self, expected: &Self::Consume, _index: usize, token: &Self::Token) -> bool {
        self.is_whitespace = token.is_whitespace();
        expected == token
    }

    fn peek(&mut self, args: &Self::Peek, index: usize, token: Option<&Self::Token>) -> bool {
        match args {
            Peek::WordBoundary => {
                token.map_or(true, |tok| tok.is_whitespace() ^ self.is_whitespace)
            }
            Peek::Save(slot) => {
                self.saves[*slot] = Some(index);
                true
            }
        }
    }

    fn any(&mut self, _index: usize, token: &Self::Token) -> bool {
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
    use self::program::Instr;
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
    use self::program::Instr;
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
