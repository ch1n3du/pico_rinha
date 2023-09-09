use pico_rinha::{ast::Program, walker};

fn main() {
    let raw_fib_program: String = std::fs::read_to_string("files/fib.json").unwrap();
    let fib_program: Program = serde_json::from_str(&raw_fib_program).unwrap();
    let mut tree_walking_interpreter: walker::Walker = walker::Walker::new();
    let result: walker::Value = tree_walking_interpreter.interpret(&fib_program).unwrap();

    println!("Result: {result}");
}
