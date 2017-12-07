#![feature(box_syntax)]
#![feature(box_patterns)]

#![feature(slice_patterns)]

mod lexer;
mod parser;
mod interpreter;

fn main() {
    let mut chars = "3".chars();
    println!("{:?}", chars.next().unwrap().to_digit(32));
}
