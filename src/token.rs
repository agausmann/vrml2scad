use chumsky::error::Simple;
use chumsky::primitive::{choice, filter, just, take_until};
use chumsky::text::{digits, ident, newline};
use chumsky::Parser as _;

pub trait Parser<T>: chumsky::Parser<char, T, Error = Simple<char>> {}
impl<T, P: chumsky::Parser<char, T, Error = Simple<char>>> Parser<T> for P {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Comma,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    Number(String),
    Ident(String),
}

impl Token {
    pub fn tokenizer() -> impl Parser<Vec<Self>> {
        whitespace().ignore_then(Self::parser().then_ignore(whitespace()).repeated())
    }

    pub fn parser() -> impl Parser<Self> {
        choice((
            just(',').to(Self::Comma),
            just('[').to(Self::OpenBracket),
            just(']').to(Self::CloseBracket),
            just('{').to(Self::OpenBrace),
            just('}').to(Self::CloseBrace),
            number().map(Self::Number),
            ident().map(Self::Ident),
        ))
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn number() -> impl Parser<String> {
    // [+-]?([0-9]+(\.[0-9]*)?|[0-9]*\.[0-9]+)
    choice((just('+'), just('-')))
        .or_not()
        .chain::<char, _, _>(choice((
            digits(10).chain::<char, _, _>(
                just('.')
                    .chain::<char, _, _>(
                        digits(10).or_not().map(|opt| opt.unwrap_or(String::new())),
                    )
                    .or_not()
                    .flatten(),
            ),
            digits(10)
                .or_not()
                .map(|opt| opt.unwrap_or(String::new()))
                .chain::<char, _, _>(just('.'))
                .chain::<char, _, _>(digits(10)),
        )))
        .collect::<String>()
}

fn whitespace() -> impl Parser<()> {
    choice((filter(|c: &char| c.is_whitespace()).ignored(), comment()))
        .repeated()
        .ignored()
}

fn comment() -> impl Parser<()> {
    just('#').then(take_until(newline())).ignored()
}
