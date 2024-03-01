use std::process::exit;

mod lex;
mod parser;
mod ast;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let [_, file] = args.as_slice() else {
        eprintln!("usage: simplc [file]");
        exit(0);
    };

    let file = std::fs::read_to_string(file).unwrap();
    let tokens = lex::tokenize(file.as_str()).expect("parse errors found!");
    println!("{:#?}", tokens);
}
