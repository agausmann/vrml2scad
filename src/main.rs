mod node;
mod token;

use self::node::{DefOr, Node};
use self::token::tokenize;
use chumsky::Parser;
use std::io::{stdin, stdout, Read};

fn main() {
    let mut input = String::new();
    stdin().read_to_string(&mut input).unwrap();

    let tokens = tokenize(&input);
    //eprintln!("{:?}", tokens);

    let parser = DefOr::<Node>::parser(Node::parser());
    let node = parser.parse(tokens).unwrap();
    //eprintln!("{:?}", node);

    node.write_def(&mut stdout()).unwrap();
    println!("{}();", node.id());
}
