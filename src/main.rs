use pico_rinha::{ast::Program, vm::Compiler};

fn main() {
    let raw_fib_program: String = std::fs::read_to_string("files/fib.json").unwrap();
    let fib_program: Program = serde_json::from_str(&raw_fib_program).unwrap();

    use pico_rinha::walker;

    let mut tree_walking_interpreter: walker::Walker = walker::Walker::new();
    let result: walker::Value = tree_walking_interpreter.interpret(&fib_program).unwrap();
    println!("Walker Result: {result}");

    use pico_rinha::vm;
    let main_function: vm::Function = Compiler::new().compile(fib_program);
    let result: vm::Value = vm::VM::new(main_function).interpret();
    println!("VM Result: {result}");
}
