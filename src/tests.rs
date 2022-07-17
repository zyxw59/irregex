use crate::{engine, program};

#[derive(Clone, Debug, Hash)]
pub struct Engine {
    saves: Vec<Option<usize>>,
    is_word: bool,
}

impl Engine {
    fn new(num_slots: usize) -> Self {
        Engine {
            saves: vec![None; num_slots],
            is_word: false,
        }
    }
}

impl engine::Engine for Engine {
    type Token = char;
    type Consume = Consume;
    type Peek = Peek;

    fn consume(&mut self, args: &Self::Consume, _index: usize, token: &Self::Token) -> bool {
        self.is_word = !token.is_whitespace();
        match args {
            Consume::Any => true,
            Consume::Token(expected) => expected == token,
        }
    }

    fn peek(&mut self, args: &Self::Peek, index: usize, token: Option<&Self::Token>) -> bool {
        match args {
            Peek::WordBoundary => token
                .as_ref()
                .map_or(true, |tok| !tok.is_whitespace() ^ self.is_word),
            Peek::Save(slot) => {
                self.saves[*slot] = Some(index);
                true
            }
        }
    }
}

pub enum Consume {
    Any,
    Token(char),
}

pub enum Peek {
    WordBoundary,
    Save(usize),
}

#[test]
fn program() {
    use self::program::Instr;
    // /(ab?)(b?c)\b/
    let prog: Vec<program::Instr<Engine>> = vec![
        // 0: *? quantifier
        Instr::JSplit(3),
        // 1: match a token
        Instr::Consume(Consume::Any),
        // 2: repeat
        Instr::Jump(0),
        // 3: save start of match
        Instr::Peek(Peek::Save(0)),
        // 4: save start of first subgroup
        Instr::Peek(Peek::Save(2)),
        // 5: a
        Instr::Consume(Consume::Token('a')),
        // 6: optional b
        Instr::Split(8),
        // 7: b
        Instr::Consume(Consume::Token('b')),
        // 8: save end of first subgroup
        Instr::Peek(Peek::Save(3)),
        // 9: save start of second subgroup
        Instr::Peek(Peek::Save(4)),
        // 10: optional b
        Instr::Split(12),
        // 11: b
        Instr::Consume(Consume::Token('b')),
        // 12: c
        Instr::Consume(Consume::Token('c')),
        // 13: save end of second subgroup
        Instr::Peek(Peek::Save(5)),
        // 14: word boundary
        Instr::Peek(Peek::WordBoundary),
        // 15: save end of match
        Instr::Peek(Peek::Save(1)),
        // 16: end of match
        Instr::Match,
    ];
    let program = program::Program { prog };
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
    // /.*?(ab|b)/
    let prog: Vec<program::Instr<Engine>> = vec![
        // 0: don't repeat if possible
        Instr::JSplit(3),
        // 1: .
        Instr::Consume(Consume::Any),
        // 2: try to repeat
        Instr::Jump(0),
        // 3: start of match
        Instr::Peek(Peek::Save(0)),
        // 4: ab|b
        Instr::Split(6),
        // 5: a
        Instr::Consume(Consume::Token('a')),
        // 6: b
        Instr::Consume(Consume::Token('b')),
        // 7: jump to end of match
        Instr::Jump(9),
        // 8: b
        Instr::Consume(Consume::Token('b')),
        // 9: end of match
        Instr::Peek(Peek::Save(1)),
    ];
    let program = program::Program { prog };
    let saves = program.exec(Engine::new(2), "ab".chars());
    // 'ab' matches first, even tho 'b' has higher precedence, because 'ab' starts earlier
    assert_eq!(
        saves.iter().map(|engine| &engine.saves).collect::<Vec<_>>(),
        &[&[Some(0), Some(2)], &[Some(1), Some(2)]],
    );
}
