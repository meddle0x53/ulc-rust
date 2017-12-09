#![feature(box_syntax)]
#![feature(box_patterns)]

#![feature(slice_patterns)]

extern crate rustyline;

mod lexer;
mod parser;
mod interpreter;
mod repl;

fn main() {
    println!("\nWelcome to the Untyped Lambda Calculus REPL!");
    repl::start("> ", (|s| interpreter::run(&s)))
}

