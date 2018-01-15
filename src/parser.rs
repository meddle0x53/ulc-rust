use std::fmt;
use std::slice::Iter;
use std::iter::Peekable;
use std::collections::HashMap;

use lexer::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Variable(u32, u32),
    Abstraction(String, Box<Term>),
    Application(Box<Term>, Box<Term>)
}

pub type Runtime = HashMap<String, Term>;

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    NameBind
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContextEntry {
    pub name: String,
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

#[derive(PartialEq)]
pub struct ParseError {
    pub message: String,
}

impl Context {
    pub fn new() -> Context {
        Context{entries: Vec::new()}
    }

    fn index_to_name(&self, index: u32) -> Result<ContextEntry, IndexingError> {
        if index >= (self.entries.len() as u32) {
            Err(IndexingError::BadIndex(index, self.entries.len() as u32))
        } else {
            Ok((&self.entries[(index) as usize]).clone())
        }
    }

    fn name_to_index(&self, name: &str) -> Result<u32, ParseError> {
        fn walk(slc: &[ContextEntry], name: String, i: u32) -> Result<u32, ParseError> {
            match *slc {
                [] => Err(ParseError{message: format!("Unbound var {}", name)}),
                [ref head, ref tail..] => {
                    if head.name == name {
                        Ok(i)
                    } else {
                        walk(tail, name, i + 1)
                    }
                }
            }
        }

        walk(self.entries.as_slice(), name.to_owned(), 0)
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

    pub fn pick_fresh_name(&self, var: &str) -> (Context, ContextEntry) {
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

    pub fn is_index_bound(&self, index: u32) -> bool {
        index < (self.entries.len() as u32)
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
                    f, "λ{}.{}", var1, TermWithContext {
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

use lexer::Token::*;
use self::Term::*;

pub fn parse(tokens: Vec<Token>, runtime: &mut Runtime) -> Result<Vec<Term>, ParseError> {
    if tokens.is_empty() { parse_error!("Empty expression") }

    //let de_bruijn_tokens = convert_to_de_bruijn_tokens(&tokens);

    let mut vec = Vec::new();
    let mut token_iterator: Peekable<Iter<Token>> =
        tokens.iter().peekable();

    let context = &Context::new();
    while let Some(token) = token_iterator.next() {
        vec.push(parse_term_from_tokens(token, &mut token_iterator, context, runtime)?);
    }

    Ok(vec)
}

fn parse_term_from_tokens(
    token: &Token, tokens: &mut Peekable<Iter<Token>>, context: &Context,
    runtime: &mut Runtime
) -> Result<Term, ParseError> {
    match token {
        &Lambda(ref name) => {
            if let Some(next_token) = tokens.next() {
                let (new_context, var1) = context.pick_fresh_name(name);
                Ok(Abstraction(
                        var1.name.to_owned(),
                        box parse_term_from_tokens(
                            next_token, tokens, &new_context, runtime
                        )?
                ))
            } else {
                parse_error!("Lambda without a term")
            }
        },
        &Identifier(ref name) => {
            let index = context.name_to_index(name)?;
            Ok(Variable(index, context.entries.len() as u32))
        },
        &OpenParen => {
            if let Some(token1) = tokens.next() {
                let term1 = parse_term_from_tokens(token1, tokens, context, runtime)?;
                if let Some(token2) = tokens.next() {
                    let term2 = parse_term_from_tokens(token2, tokens, context, runtime)?;
                    if let Some(&CloseParen) = tokens.next() {
                        Ok(Application(box term1, box term2))
                    } else {
                        parse_error!("Invalid expression : Openning paren has no correspondent closing one")
                    }
                } else {
                    parse_error!("Invalid expression ending on openning paren")
                }

            } else {
                parse_error!("Invalid expression ending on openning paren")
            }
        },
        &CloseParen => {
            parse_error!("Invalid expression : Closing paren has no correspondent openning one")
        },
        &Let(ref name) => {
            if let Some(next_token) = tokens.next() {
                let term = parse_term_from_tokens(next_token, tokens, context, runtime)?;
                runtime.insert(name.to_string(), term.clone());
                Ok(term)
            } else {
                parse_error!("Let without a right side")
            }
        },
        &Binding(ref name) => {
            if let Some(term) = runtime.get(name) {
                Ok(term.clone())
            } else {
                parse_error!("Unknown binding: {}", name)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Term::*;
    use super::Runtime;
    use super::parse;

    use lexer::Token;

    // Construct helpers
    fn lambda(arg: &str) -> Token { Token::Lambda(String::from(arg)) }
    fn var(arg: &str) -> Token { Token::Identifier(String::from(arg)) }

    #[test]
    fn parse_simple_abstractions_test() {
        assert_eq!(
            parse(vec![lambda("x"), var("x")], &mut Runtime::new()).ok().unwrap(),
            vec![Abstraction("x".to_string(), box Variable(0, 1))]
        );

        assert_eq!(
            parse(vec![
                  lambda("x"), lambda("y"),
                  Token::OpenParen, Token::OpenParen,
                  var("x"), var("y"),
                  Token::CloseParen,
                  var("x"),
                  Token::CloseParen
            ], &mut Runtime::new()).ok().unwrap(),
            vec![
                Abstraction(
                    "x".to_string(),
                    box Abstraction(
                        "y".to_string(),
                        box Application(
                            box Application(
                                box Variable(1, 2), box Variable(0, 2)
                            ),
                            box Variable(1, 2)
                        )
                    )
                )
            ]
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
        assert_eq!(term.to_string(), "λx.λx'.(x' x)");

        let term = Abstraction("x".to_string(), box Abstraction("y".to_string(), box Application(box Application(box Variable(0, 2), box Variable(1, 2)), box Variable(0, 2))));
        assert_eq!(term.to_string(), "λx.λy.((y x) y)");
    }

}
