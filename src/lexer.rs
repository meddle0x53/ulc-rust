use std::fmt;
use std::str::Chars;
use std::iter::{Peekable, Enumerate};

#[derive(Debug, PartialEq)]
pub enum Token {
    Lambda(String),
    OpenParen,
    CloseParen,
    Number(u32, u32)
}

#[derive(Clone, Debug, PartialEq)]
enum NamedToken {
    Lambda(String),
    OpenParen,
    CloseParen,
    Identifier(String)
}

use self::Token::*;

pub struct SyntaxError {
    pub message: String,
    position: usize
}

impl fmt::Debug for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SyntaxError: {} (position: {})", self.message, self.position)
    }
}

macro_rules! syntax_error {
    ($position:ident, $($arg:tt)*) => (
        return Err(SyntaxError { message: format!($($arg)*), position: $position + 1 })
    )
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, SyntaxError> {
    match tokenize_named(input) {
        Ok(named_tokens) => Ok(convert_named_tokens(&named_tokens)),
        Err(error) => Err(error)
    }
}

fn convert_named_tokens(named_tokens: &[NamedToken]) -> Vec<Token> {
    do_convert_named_tokens(named_tokens, &mut Vec::new(), &mut 0)
}

fn do_convert_named_tokens(named_tokens: &[NamedToken], stack: &mut Vec<String>, position: &mut usize) -> Vec<Token> {
    let mut tokens = Vec::with_capacity(named_tokens[*position..].len());
    let mut counter = 0;

    while let Some(token) = named_tokens.get(*position) {
        match *token {
            NamedToken::Lambda(ref name) => {
                tokens.push(Lambda((*name).clone()));
                stack.push(name.to_owned());
                counter += 1;
            },
            NamedToken::OpenParen => {
                tokens.push(OpenParen);
                *position += 1;
                tokens.append(&mut do_convert_named_tokens(named_tokens, stack, position));
            },
            NamedToken::CloseParen => {
                tokens.push(CloseParen);
                for _ in 0..counter { stack.pop(); }
                return tokens
            },
            NamedToken::Identifier(ref name) => {
                if let Some(index) = stack.iter().rev().position(|t| t == name) {
                    tokens.push(Number((index) as u32, stack.len() as u32))
                } else {
                    tokens.push(Number((stack.len()) as u32, stack.len() as u32))
                }
            }
        }
        *position += 1;
    }

    tokens
}

fn tokenize_named(input: &str) -> Result<Vec<NamedToken>, SyntaxError> {
    let mut chars = input.chars().enumerate().peekable();
    let mut tokens = Vec::new();

    while let Some((index, character)) = chars.next() {
        match character {
            '\\' | 'λ' | 'л' => {
                create_named_lambda(&mut chars, &mut tokens)?
            },
            '(' => { tokens.push(NamedToken::OpenParen) },
            ')' => { tokens.push(NamedToken::CloseParen) },
            _  => {
                if character.is_whitespace() {
                    ()
                } else if character == 'l' && (&input.chars().count() - index) > 7 && (&input[index..(index + 7)] == "lambda ") {
                    for _ in 1..7 { chars.next(); };
                    create_named_lambda(&mut chars, &mut tokens)?
                } else if is_valid_var(character as u8) {
                    let mut name = character.to_string();
                    while let Some(&(_, c)) = chars.peek() {
                        if c.is_whitespace() || c == '(' || c == ')' {
                            break
                        } else {
                            name.push(c);
                            chars.next();
                        }
                    }
                    tokens.push(NamedToken::Identifier(name))
                } else {
                    let position = index + 1;
                    syntax_error!(position, "Invalid character : {}", character)
                }
            }
        }
    }

    Ok(tokens)
}

fn is_valid_var(c: u8) -> bool {
    (c >= 0x41 && c <= 0x5A) || (c >= 0x61 && c <= 0x7A) || c == 0x27
}

fn create_named_lambda(chars: &mut Peekable<Enumerate<Chars>>, tokens: &mut Vec<NamedToken>) -> Result<(), SyntaxError> {
    let mut name = String::new();

    while let Some((index, character)) = chars.next() {
        if character == '.' {
            break
        } else if is_valid_var(character as u8) {
            name.push(character)
        } else {
            let position = index + 1;
            syntax_error!(position, "Invalid character : {}", character)
        }
    }
    tokens.push(NamedToken::Lambda(name));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::NamedToken;
    use super::Token::*;

    use super::tokenize_named;
    use super::convert_named_tokens;

    // Construct helpers
    fn lambda(arg: &str) -> NamedToken { NamedToken::Lambda(String::from(arg)) }
    fn var(arg: &str) -> NamedToken { NamedToken::Identifier(String::from(arg)) }

    #[test]
    fn convert_named_tokens_test() {
        assert_eq!(
            convert_named_tokens(&vec![lambda("x"), var("x")]),
            vec![Lambda("x".to_string()), Number(0, 1)]
        );
        assert_eq!(
            convert_named_tokens(&vec![lambda("x"), lambda("y"), var("y"), var("x")]),
            vec![Lambda("x".to_string()), Lambda("y".to_string()), Number(0, 2), Number(1, 2)]
        );
        assert_eq!(
            convert_named_tokens(&vec![lambda("x"), lambda("y"), var("y"), var("x"), var("z")]),
            vec![Lambda("x".to_string()), Lambda("y".to_string()), Number(0, 2), Number(1, 2), Number(2, 2)]
        );
    }


    #[test]
    fn tokenize_named_simple_abstractions_test() {
        assert_eq!(
            tokenize_named("lambda x.x").ok().unwrap(),
            vec![lambda("x"), var("x")]
        );
        assert_eq!(
            tokenize_named("\\x.x").ok().unwrap(),
            vec![lambda("x"), var("x")]
        );
        assert_eq!(
            tokenize_named("λx.y x").ok().unwrap(),
            vec![lambda("x"), var("y"), var("x")]
        );
        assert_eq!(
            tokenize_named("λx.лy.y x").ok().unwrap(),
            vec![lambda("x"), lambda("y"), var("y"), var("x")]
        );
        assert_eq!(
            tokenize_named("λx.лy.y x z'").ok().unwrap(),
            vec![lambda("x"), lambda("y"), var("y"), var("x"), var("z'")]
        );
        assert_eq!(
            tokenize_named("(λx.y)").ok().unwrap(),
            vec![NamedToken::OpenParen, lambda("x"), var("y"), NamedToken::CloseParen]
        );
    }

    #[test]
    fn tokenize_named_simple_applications_test() {
        assert_eq!(
            tokenize_named("(lambda x.x)y").ok().unwrap(),
            vec![NamedToken::OpenParen, lambda("x"), var("x"), NamedToken::CloseParen, var("y")]
        );
    }
}
