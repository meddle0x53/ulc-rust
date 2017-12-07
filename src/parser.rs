use std::fmt;
use std::slice::Iter;
use std::iter::Peekable;

use lexer::Token;
use lexer::Token::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Variable(u32, u32),
    Abstraction(String, Box<Term>),
    Application(Box<Term>, Box<Term>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    NameBind
}

#[derive(Debug, Clone, PartialEq)]
struct ContextEntry {
    name: String,
    binding: Binding,
}

impl fmt::Display for ContextEntry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Default, Clone)]
pub struct Context {
    entries: Vec<ContextEntry>,
}

#[derive(Debug, PartialEq)]
pub enum IndexingError {
    BadIndex(u32, u32)
}

impl Context {
    fn index_to_name(&self, index: u32) -> Result<ContextEntry, IndexingError> {
        println!("{:?}", index);
        println!("{:?}", self.entries);
        if index >= (self.entries.len() as u32) {
            Err(IndexingError::BadIndex(index, self.entries.len() as u32))
        } else {
            Ok((&self.entries[(index) as usize]).clone())
        }
    }

    fn is_name_bound(&self, name: &str) -> bool {
        fn walk(entries: &[ContextEntry], name: String) -> bool {
            match *entries {
                [] => false,
                [ref head, ref tail..] => {
                    if head.name == name {
                        true
                    } else {
                        walk(tail, name)
                    }
                }
            }
        }

        walk(self.entries.as_slice(), name.to_owned())
    }

    fn pick_fresh_name(&self, var: &str) -> (Context, ContextEntry) {
        let name = var.to_owned();

        if self.is_name_bound(var) {
            self.pick_fresh_name(&(name + "'"))
        } else {
            let entry = ContextEntry {
                name: name, binding: Binding::NameBind,
            };

            let mut new_context = (*self).clone();
            new_context.entries.insert(0, entry.clone());

            (new_context, entry)
        }
    }
}


struct TermWithContext<'a> {
    context: &'a Context,
    term: &'a Term
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            TermWithContext {
                context: &Context{entries: Vec::new()}, term: self,
            }
        )
    }
}

impl<'a> fmt::Display for TermWithContext<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.term {
            Variable(index, _) => {
                match self.context.index_to_name(index) {
                    Ok(entry) => write!(f, "{}", entry),
                    Err(e) => write!(f, "{:?}", e),
                }
            },
            Abstraction(ref var, ref term) => {
                let (new_context, var1) = self.context.pick_fresh_name(var);
                write!(
                    f, "(lambda {}. {})", var1, TermWithContext {
                        context: &new_context, term: term
                    }
                )
            },
            Application(ref term1, ref term2) => {
                write!(
                    f, "({} {})",
                    TermWithContext {
                        context: self.context,
                        term: term1,
                    },
                    TermWithContext {
                        context: self.context,
                        term: term2,
                    }
                )
            }
        }
    }
}

#[derive(PartialEq)]
pub struct ParseError {
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {}", self.message)
    }
}
impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError: {}", self.message)
    }
}

macro_rules! parse_error {
    ($($arg:tt)*) => (
        return Err(ParseError { message: format!($($arg)*)})
    )
}

use self::Term::*;

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Term>, ParseError> {
    if tokens.is_empty() { parse_error!("Empty expression") }


    let mut vec = Vec::new();
    let mut token_iterator: Peekable<Iter<Token>> = tokens.iter().peekable();

    while let Some(token) = token_iterator.next() {
        vec.push(parse_term_from_tokens(token, &mut token_iterator)?);
    }

    Ok(vec)
}

fn parse_term_from_tokens(token: &Token, tokens: &mut Peekable<Iter<Token>>) -> Result<Term, ParseError> {
    match token {
        &Lambda(ref name) => {
            if let Some(next_token) = tokens.next() {
                Ok(Abstraction(name.to_owned(), box parse_term_from_tokens(next_token, tokens)?))
            } else {
                parse_error!("Lambda without a term")
            }
        },
        &Number(index, check) => {
            Ok(Variable(index, check))
        },
        &OpenParen => {
            if let Some(token1) = tokens.next() {
                match token1 {
                    &Lambda(ref name) => {
                        if let Some(next_token) = tokens.next() {
                            if let Some(&CloseParen) = tokens.next() {
                                Ok(Abstraction(name.to_owned(), box parse_term_from_tokens(next_token, tokens)?))
                            } else {
                                parse_error!("Invalid expression : Openning paren has no correspondent closing one")
                            }
                        } else {
                            parse_error!("Lambda without a term")
                        }
                    },
                    _ => {
                        let term1 = parse_term_from_tokens(token1, tokens)?;
                        if let Some(token2) = tokens.next() {
                            let term2 = parse_term_from_tokens(token2, tokens)?;
                            if let Some(&CloseParen) = tokens.next() {
                                Ok(Application(box term1, box term2))
                            } else {
                                parse_error!("Invalid expression : Openning paren has no correspondent closing one")
                            }
                        } else {
                            parse_error!("Invalid expression ending on openning paren")
                        }
                    }
                }

            } else {
                parse_error!("Invalid expression ending on openning paren")
            }
        },
        &CloseParen => {
            parse_error!("Invalid expression : Closing paren has no correspondent openning one")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Term::*;
    use super::parse;

    use lexer::Token::*;

    #[test]
    fn parse_simple_variable_test() {
        assert_eq!(
            parse(vec![Number(1, 1)]).ok().unwrap(),
            vec![Variable(1, 1)]
        );
    }

    #[test]
    fn parse_simple_abstractions_test() {
        assert_eq!(
            parse(vec![Lambda("x".to_string()), Number(0, 1)]).ok().unwrap(),
            vec![Abstraction("x".to_string(), box Variable(0, 1))]
        );

        assert_eq!(
            parse(vec![Lambda("x".to_string()), Lambda("y".to_string()), OpenParen, OpenParen, Number(0, 2), Number(1, 2), CloseParen, Number(2, 2), CloseParen]).ok().unwrap(),
            vec![Abstraction("x".to_string(), box Abstraction("y".to_string(), box Application(box Application(box Variable(0, 2), box Variable(1, 2)), box Variable(2, 2))))]
        );
    }

    #[test]
    fn display_term_test() {
        let term = Abstraction(
            "x".to_string(),
            box Abstraction(
                "x".to_string(),
                box Application(box Variable(0, 2), box Variable(1, 2)),
            ),
        );
        assert_eq!(term.to_string(), "(lambda x. (lambda x'. (x' x)))");

        let term = Abstraction("x".to_string(), box Abstraction("y".to_string(), box Application(box Application(box Variable(0, 2), box Variable(1, 2)), box Variable(0, 2))));
        assert_eq!(term.to_string(), "(lambda x. (lambda y. ((y x) y)))");
    }
}
