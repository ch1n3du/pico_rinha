pub fn run() {
    let arguments: Vec<String> = std::env::args().collect();

    if arguments.len() < 3 {
        println!("Error: This CLI expects at least two (2) arguments.");
        print_help_message();
        return;
    }

    let interpreter_choice: &String = &arguments[1];
    let file_content: String = match std::fs::read_to_string(&arguments[2]) {
        Ok(content) => content,
        Err(error) => {
            println!("Error: {error}");
            print_help_message();
            return;
        }
    };
    let program: Program = match serde_json::from_str(&file_content) {
        Ok(program) => program,
        Err(_) => {
            println!("Error: Invalid program.");
            print_help_message();
            return;
        }
    };

    match interpreter_choice.as_str() {
        "walker" => {
            use crate::walker;

            let mut tree_walking_interpreter: walker::Walker = walker::Walker::new();
            let result: walker::Value = tree_walking_interpreter.interpret(&program).unwrap();
            println!("Walker Result: {result}");
        }
        "vm" => {
            use crate::vm;

            let main_function: vm::Function = Compiler::new().compile(program);
            let result: vm::Value = match vm::VM::new(main_function).interpret() {
                Ok(value) => value,
                Err(error) => {
                    print!("{error}");
                    return;
                }
            };
            println!("VM Result: {result:?}");
        }
        _ => {
            println!("Error: The first CLI argument specifies the intepreter choice and is expected to be 'walker' or 'vm'.");
            print_help_message();
            return;
        }
    }
}

use crate::{ast::Program, vm::Compiler};

fn print_help_message() {
    println!("Guide:");
    let indent: String = " ".repeat(2);
    println!(
        "{indent}pico_rinha walker <file_name>            -- Runs the file using the tree-walking interpreter.",
    );
    println!(
        "{indent}pico_rinha gotta_go_fast <file_name>     -- Runs the file using the bytecode interpreter."
    );
}
