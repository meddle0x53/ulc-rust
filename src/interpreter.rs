use std::fmt;

use lexer::tokenize;
use lexer::SyntaxError;

use parser::parse;
use parser::Term;
use parser::Term::*;
use parser::Context;
use parser::ParseError;
use parser::Runtime;

#[derive(PartialEq)]
pub struct EvalError {
    message: String
}

impl fmt::Debug for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "EvalError : {}", self.message)
    }
}

macro_rules! eval_error {
    ($($arg:tt)*) => (
        return Err(EvalError { message: format!($($arg)*) })
    )
}

pub fn run(source: &str, runtime: &mut Runtime) -> Result<String, String> {
    let tokens = match tokenize(source) {
        Ok(res) => res,
        Err(SyntaxError{message: msg, ..}) => return Err(msg)
    };
    let terms = match parse(tokens, runtime) {
        Ok(res) => res,
        Err(ParseError{message: msg}) => return Err(msg)
    };
    let values = terms.iter()
        .map(|t| eval(&t, &Context::new()))
        .map(|t| t.to_string())
        .fold("".to_string(), |acc, v| if acc.is_empty() {
            v
        } else {
            format!("{}\n{}", acc, v)
        });

    Ok(values)
}

pub fn eval(term: &Term, ctx: &Context) -> Term {
    match eval_step(term, ctx, true) {
        Ok(term1) => eval(&term1, ctx),
        Err(EvalError{ message: _ }) => term.clone(),
    }
}

fn eval_step(term: &Term, ctx: &Context, cont: bool) -> Result<Term, EvalError> {
    match term {
        &Application(box Abstraction(_, ref term1), ref term2) if is_value(&term2, ctx) => {
            Ok(substitute(term1, term2))
        },
        &Application(ref term1, ref term2) if is_value(term1, ctx) => {
            Ok(Application(term1.clone(), box eval_step(term2, ctx, cont)?))
        },
        &Application(ref term1, ref term2) => {
            Ok(Application(box eval_step(term1, ctx, cont)?, term2.clone()))
        },
        &Abstraction(ref name, ref term1) => {
            let (new_ctx, entry) = ctx.pick_fresh_name(name);
            Ok(Abstraction(entry.name, box eval_step(term1, &new_ctx, true)?))
        },
        _ => eval_error!("No rule applies!")
    }
}

fn is_value(term: &Term, ctx: &Context) -> bool {
    match term {
        &Abstraction(_, _) => true,
        &Variable(_, _) => true,
        &Application(box Variable(var1, _), ref term2) => {
            if ctx.is_index_bound(var1) && is_value(term2, ctx) {
                true
            } else {
                false
            }
        },
        _ => false
    }
}

fn substitute(term: &Term, substitution: &Term) -> Term {
    shift_term(
        &substitute_term(term, &shift_term(substitution, 1), 0), -1
    )
}

fn shift_term(term: &Term, d: i32) -> Term {
    do_shift_term(term, d, 0)
}

fn do_shift_term(term: &Term, d: i32, c: i32) -> Term {
    match term {
        &Variable(index, n) => {
            if index as i32 >= c {
                Variable((index as i32 + d) as u32, (n as i32 + d) as u32)
            } else {
                Variable(index, (n as i32 + d) as u32)
            }
        },
        &Abstraction(ref hint, ref term1) => {
            Abstraction(hint.clone(), box do_shift_term(term1, d, c + 1))
        },
        &Application(ref term1, ref term2) => {
            Application(
                box do_shift_term(term1, d, c), box do_shift_term(term2, d, c)
            )
        }
    }
}

fn substitute_term(term: &Term, substitution: &Term, j: i32) -> Term {
    do_substitute_term(term, substitution, j, 0)
}

fn do_substitute_term(term: &Term, substitution: &Term, j: i32, c: i32) -> Term {
    match term {
        &Variable(index, n) => {
            if index as i32 == j + c {
                shift_term(substitution, c)
            } else {
                Variable(index, n)
            }
        },
        &Abstraction(ref hint, ref term1) => {
            Abstraction(
                hint.clone(),
                box do_substitute_term(term1, substitution, j, c + 1)
            )
        },
        &Application(ref term1, ref term2) => {
            Application(
                box do_substitute_term(term1, substitution, j, c),
                box do_substitute_term(term2, substitution, j, c)
            )
        }
    }
}
