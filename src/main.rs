#![feature(box_syntax)]
#![feature(box_patterns)]

#![feature(slice_patterns)]

extern crate rustyline;
extern crate colored;

mod lexer;
mod parser;
mod interpreter;
mod repl;

use colored::*;

fn main() {
    println!("{}", "\nInteractive Untyped Lambda Calculus (0.1.0) - press Ctrl+C to exit\n".blue());
    repl::start("> ", (|s| interpreter::run(&s)))
}

