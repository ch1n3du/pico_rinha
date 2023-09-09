use self::chunk::{Chunk, OpCode, Value};

mod chunk;
mod compiler;

#[derive(Debug)]
struct VM {
    instruction_pointer: usize,
    value_stack: Vec<Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            instruction_pointer: 0,
            value_stack: Vec::with_capacity(256),
        }
    }

    /// Fetch, Decode, Execute loop
    pub fn interpret(&mut self, chunk: &Chunk) -> Value {
        loop {
            // Fetch the current instruction
            let Some(raw_instruction): Option<u8> = chunk.get_instruction(self.instruction_pointer)
            else {
                panic!("VM Error: No more instructions to execute ")
            };

            // Decode the instruction
            let Some(op_code) = OpCode::from_u8(raw_instruction) else {
                panic!("VM Error: Invalid instruction '{raw_instruction}'.")
            };

            println!(
                "(IP = {}) Interpreting {op_code:?}",
                self.instruction_pointer
            );
            // Execute the instruction
            match op_code {
                OpCode::Return => return self.value_stack.pop().unwrap(),
                OpCode::GetConstant => self.get_constant(chunk),
                OpCode::Add => self.add(),
                _ => todo!(),
            }

            self.instruction_pointer += 1 + op_code.arity();
        }
    }

    fn get_constant(&mut self, chunk: &Chunk) {
        let constant_index: usize =
            chunk.get_instruction(self.instruction_pointer + 1).unwrap() as usize;
        let constant: Value = chunk.get_constant(constant_index).unwrap();
        self.value_stack.push(constant);
    }

    fn add(&mut self) {
        println!("VM before calling 'Add': {self:#?}");
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();

        use Value::*;
        let result = match (arg_1, arg_2) {
            (Int(int_1), Int(int_2)) => Value::Int(int_1 + int_2),
            (String(string_1), String(string_2)) => Value::String(format!("{string_1}{string_2}")),
            (arg_1, arg_2) => panic!(
                "VM Error: Addition is not supported between '{}' and '{}'",
                arg_1.type_of(),
                arg_2.type_of()
            ),
        };

        self.push(result);
        println!("VM after calling 'Add': {self:#?}");
    }

    fn sub(&mut self) {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result = match (arg_1, arg_2) {
            (Value::Int(int_1), Value::Int(int_2)) => Value::Int(int_1 - int_2),
            (arg_1, arg_2) => panic!(
                "VM Error: Subtraction is not supported between '{}' and '{}'",
                arg_1.type_of(),
                arg_2.type_of()
            ),
        };

        self.push(result);
    }

    fn mul(&mut self) {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result = match (arg_1, arg_2) {
            (Value::Int(int_1), Value::Int(int_2)) => Value::Int(int_1 * int_2),
            (arg_1, arg_2) => panic!(
                "VM Error: Multiplication is not supported between '{}' and '{}'",
                arg_1.type_of(),
                arg_2.type_of()
            ),
        };

        self.push(result);
    }

    fn div(&mut self) {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result = match (arg_1, arg_2) {
            (Value::Int(int_1), Value::Int(int_2)) => Value::Int(int_1 / int_2),
            (arg_1, arg_2) => panic!(
                "VM Error: Division is not supported between '{}' and '{}'",
                arg_1.type_of(),
                arg_2.type_of()
            ),
        };

        self.push(result);
    }

    fn eq(&mut self) {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result: Value = Value::Bool(arg_1.eq(&arg_2));

        self.push(result);
    }

    fn not_eq(&mut self) {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result: Value = Value::Bool(arg_1.ne(&arg_2));

        self.push(result);
    }

    fn greater_than(&mut self) {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result: Value = Value::Bool(arg_1.gt(&arg_2));

        self.push(result);
    }

    fn less_than(&mut self) {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result: Value = Value::Bool(arg_1.lt(&arg_2));

        self.push(result);
    }

    fn logical_and(&mut self) {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result = match (arg_1, arg_2) {
            (Value::Bool(bool_1), Value::Bool(bool_2)) => Value::Bool(bool_1 && bool_2),
            (arg_1, arg_2) => panic!(
                "VM Error: '<' is not supported between '{}' and '{}'",
                arg_1.type_of(),
                arg_2.type_of()
            ),
        };

        self.push(result);
    }

    fn logical_or(&mut self) {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result = match (arg_1, arg_2) {
            (Value::Bool(bool_1), Value::Bool(bool_2)) => Value::Bool(bool_1 || bool_2),
            (arg_1, arg_2) => panic!(
                "VM Error: '<' is not supported between '{}' and '{}'",
                arg_1.type_of(),
                arg_2.type_of()
            ),
        };

        self.push(result);
    }

    // Constant OpCodes

    fn push_unit(&mut self) {
        self.push(Value::Unit)
    }

    fn push_true(&mut self) {
        self.push(Value::Bool(true))
    }

    fn push_false(&mut self) {
        self.push(Value::Bool(false))
    }

    // Utility Methods

    fn pop_value(&mut self) -> Value {
        self.value_stack.pop().unwrap()
    }

    fn pop_two_values(&mut self) -> (Value, Value) {
        (
            self.value_stack.pop().unwrap(),
            self.value_stack.pop().unwrap(),
        )
    }

    fn push(&mut self, value: Value) {
        self.value_stack.push(value)
    }
}

// struct CallFrame<'a> {
//     chunk: Chunk,
//     ip: u8,
//     frame_pointer: usize,
// }

#[cfg(test)]
mod tests {
    use super::{
        chunk::{Chunk, OpCode, Value},
        VM,
    };

    #[test]
    fn add_works() {
        let mut chunky = Chunk::new();
        let const_index_1: usize = chunky.add_constant(Value::Int(1));
        let const_index_2: usize = chunky.add_constant(Value::Int(2));

        // Put the args on the stack and call Add
        chunky.write_opcode_with_args(OpCode::GetConstant, &[const_index_1 as u8]);
        chunky.write_opcode_with_args(OpCode::GetConstant, &[const_index_2 as u8]);
        chunky.write_opcode(OpCode::Add);
        println!("Chunk: {chunky:#?}");

        // Return
        chunky.write_opcode(OpCode::Return);

        let mut vm = VM::new();
        let result: Value = vm.interpret(&chunky);
        assert_eq!(result, Value::Int(3))
    }
}
