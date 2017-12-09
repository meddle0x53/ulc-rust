use rustyline::Editor;
use rustyline::error::ReadlineError;

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
                    let result = f(input);
                    println!("{}", result.unwrap_or_else(|e| format!("Error: {}", e)));
                }
            },
            None => return
        };
    };
}
