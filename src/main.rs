mod node;
mod token;

use self::node::{DefOr, Node};
use self::token::Token;
use chumsky::Parser;
use std::io::{stdin, Read};

fn main() {
    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();

    let tokens = Token::tokenizer()
        .then_ignore(chumsky::primitive::end())
        .parse(input)
        .unwrap();

    //println!("{:?}", tokens);

    let parser = DefOr::<Node>::parser(Node::parser());
    let node = parser.parse(tokens).unwrap();

    println!("{:?}", node);
}
