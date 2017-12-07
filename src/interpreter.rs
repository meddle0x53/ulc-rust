use std::fmt;

use parser::Term;
use parser::Term::*;
use parser::Context;

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

pub fn eval(term: &Term, ctx: &Context) -> Term {
    match eval_step(term, ctx) {
        Ok(term1) => eval(&term1, ctx),
        Err(EvalError{ message: _ }) => term.clone(),
    }
}

fn eval_step(term: &Term, ctx: &Context) -> Result<Term, EvalError> {
    match term {
        &Application(box Abstraction(_, ref term1), ref term2) if is_value(&term2) => {
            Ok(substitute(term1, term2))
        },
        &Application(ref term1, ref term2) if is_value(term1) => {
            Ok(Application(term1.clone(), box eval_step(term2, ctx)?))
        },
        &Application(ref term1, ref term2) => {
            Ok(Application(box eval_step(term1, ctx)?, term2.clone()))
        },
        _ => eval_error!("No rule applies!")
    }
}

fn is_value(term: &Term) -> bool {
    match term {
        &Abstraction(_, _) => true,
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
