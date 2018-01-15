use std::fmt;
use std::str::Chars;
use std::iter::{Peekable, Enumerate};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Lambda(String),
    OpenParen,
    CloseParen,
    Identifier(String),
    Let(String),
    Binding(String)
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
    let mut chars = input.chars().enumerate().peekable();
    let mut tokens = Vec::new();

    while let Some((index, character)) = chars.next() {
        match character {
            '\\' | 'λ' | 'л' => {
                create_lambda(&mut chars, &mut tokens)?
            },
            '(' => { tokens.push(OpenParen) },
            ')' => { tokens.push(CloseParen) },
            _  => {
                if character.is_whitespace() {
                    ()
                } else if character == 'l' && (&input.chars().count() - index) > 7 && (&input[index..(index + 7)] == "lambda ") {
                    for _ in 1..7 { chars.next(); };
                    create_lambda(&mut chars, &mut tokens)?
                } else if character == 'l' && (&input.chars().count() - index) > 4 && (&input[index..(index + 4)] == "let ") {
                    for _ in 1..4 { chars.next(); };
                    create_let(&mut chars, &mut tokens)?
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
                    tokens.push(Identifier(name))
                } else if is_valid_binding(character as u8) {
                    let mut name = character.to_string();
                    while let Some(&(_, c)) = chars.peek() {
                        if c.is_whitespace() || c == '(' || c == ')' {
                            break
                        } else {
                            name.push(c);
                            chars.next();
                        }
                    }
                    tokens.push(Binding(name))
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
    (c >= 0x61 && c <= 0x7A) || c == 0x27 // (c >= 0x41 && c <= 0x5A)
}

fn is_valid_binding(c: u8) -> bool {
    (c >= 0x41 && c <= 0x5A)
}

fn create_let(chars: &mut Peekable<Enumerate<Chars>>, tokens: &mut Vec<Token>) -> Result<(), SyntaxError> {
    let mut name = String::new();

    while let Some((index, character)) = chars.next() {
        if !character.is_whitespace() {
            if is_valid_binding(character as u8) {
                name.push(character)
            } else {
                let position = index + 1;
                syntax_error!(position, "Invalid character : {}", character)
            }
            break
        }
    }

    while let Some((index, character)) = chars.next() {
        if character == '=' {
            break
        } else if character.is_whitespace() {
            continue
        } else if is_valid_binding(character as u8) {
            name.push(character)
        } else {
            let position = index + 1;
            syntax_error!(position, "Invalid character : {}", character)
        }
    }
    tokens.push(Let(name));
    Ok(())
}

fn create_lambda(chars: &mut Peekable<Enumerate<Chars>>, tokens: &mut Vec<Token>) -> Result<(), SyntaxError> {
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
    tokens.push(Lambda(name));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::Token;
    use super::Token::*;

    use super::tokenize;

    // Construct helpers
    fn lambda(arg: &str) -> Token { Lambda(String::from(arg)) }
    fn var(arg: &str) -> Token { Identifier(String::from(arg)) }
    fn let_token(arg: &str) -> Token { Let(String::from(arg)) }

    #[test]
    fn tokenize_simple_abstractions_test() {
        assert_eq!(
            tokenize("lambda x.x").ok().unwrap(),
            vec![lambda("x"), var("x")]
        );
        assert_eq!(
            tokenize("\\x.x").ok().unwrap(),
            vec![lambda("x"), var("x")]
        );
        assert_eq!(
            tokenize("λx.y x").ok().unwrap(),
            vec![lambda("x"), var("y"), var("x")]
        );
        assert_eq!(
            tokenize("λx.лy.y x").ok().unwrap(),
            vec![lambda("x"), lambda("y"), var("y"), var("x")]
        );
        assert_eq!(
            tokenize("λx.лy.y x z'").ok().unwrap(),
            vec![lambda("x"), lambda("y"), var("y"), var("x"), var("z'")]
        );
        assert_eq!(
            tokenize("(λx.y)").ok().unwrap(),
            vec![OpenParen, lambda("x"), var("y"), CloseParen]
        );
    }

    #[test]
    fn tokenize_simple_applications_test() {
        assert_eq!(
            tokenize("(lambda x.x)y").ok().unwrap(),
            vec![OpenParen, lambda("x"), var("x"), CloseParen, var("y")]
        );
    }

    #[test]
    fn tokenize_simple_let_test() {
        assert_eq!(
            tokenize("let I = λx.x").ok().unwrap(),
            vec![let_token("I"), lambda("x"), var("x")]
        );
    }
}
