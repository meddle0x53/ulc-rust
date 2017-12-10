use rustyline::Editor;
use rustyline::error::ReadlineError;

use colored::*;

fn ask(prompt: &str, rl: &mut Editor<()>) -> Option<String> {
    let readline = rl.readline(prompt);

    match readline {
        Ok(line) => {
            rl.add_history_entry(&line);
            Some(line)
        },
        Err(ReadlineError::Interrupted) => {
            println!("CTRL-C");
            None
        },
        Err(ReadlineError::Eof) => {
            println!("CTRL-D");
            None
        },
        Err(err) => {
            println!("Error: {:?}", err);
            None
        }
    }
}

pub fn start<F: Fn(String) -> Result<String, String>>(prompt: &str, f: F) {
    let mut rl = Editor::<()>::new();

    loop {
        match ask(prompt, &mut rl) {
            Some(input) => {
                if input.len() > 0 {
                    match f(input) {
                        Ok(result) => {
                            let output = format!("{}", result);
                            if output.contains("BadIndex") {
                                println!("{}", "Error: invalid term".red())
                            } else {
                                println!("{}", output.yellow())
                            }
                        },
                        Err(e) => println!("{}", format!("Error: {}", e).red())
                    }
                }
            },
            None => return
        };
    };
}
