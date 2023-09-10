use self::chunk::{Chunk, OpCode, Value};

mod chunk;
mod compiler;

#[derive(Debug)]
pub struct VM {
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
                OpCode::GetConstant => self.get_constant_op(chunk),
                OpCode::Print => self.print_op(),
                OpCode::LogicalNot => self.logical_not_op(),
                OpCode::Negate => self.negate_op(),
                OpCode::Add => self.add_op(),
                OpCode::Sub => self.sub_op(),
                OpCode::Mul => self.mul_op(),
                OpCode::Div => self.div_op(),
                OpCode::Eq => self.eq_op(),
                OpCode::NotEq => self.not_eq_op(),
                OpCode::GreaterThan => self.greater_than_op(),
                OpCode::LessThan => self.less_than_op(),
                OpCode::LogicalAnd => self.logical_and_op(),
                OpCode::LogicalOr => self.logical_or_op(),
                OpCode::PushTrue => self.push(Value::Bool(true)),
                OpCode::PushFalse => self.push(Value::Bool(false)),
                OpCode::PushUnit => self.push(Value::Unit),
                OpCode::GetLocal => self.get_local(chunk),
                OpCode::SetLocal => self.set_local(chunk),
            }

            self.instruction_pointer += 1 + op_code.arity();
        }
    }

    fn get_constant_op(&mut self, chunk: &Chunk) {
        let constant_index: usize =
            chunk.get_instruction(self.instruction_pointer + 1).unwrap() as usize;
        let constant: Value = chunk.get_constant(constant_index).unwrap();
        self.value_stack.push(constant);
    }

    fn print_op(&mut self) {
        let value: Value = self.pop_value();
        println!("{value}")
    }

    fn negate_op(&mut self) {
        let value = self.value_stack.last_mut().unwrap();
        if let Value::Int(int) = value {
            *value = Value::Int(-(*int));
        } else {
            panic!("VM Error: Negation (unary '-') only works for Ints.")
        }
    }

    fn logical_not_op(&mut self) {
        let value = self.value_stack.last_mut().unwrap();
        if let Value::Bool(bool) = value {
            *value = Value::Bool(!(*bool));
        } else {
            panic!("VM Error: Logical not ('!') only works for booleans.")
        }
    }

    fn add_op(&mut self) {
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
    }

    fn sub_op(&mut self) {
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

    fn mul_op(&mut self) {
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

    fn div_op(&mut self) {
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

    fn eq_op(&mut self) {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result: Value = Value::Bool(arg_1.eq(&arg_2));

        self.push(result);
    }

    fn not_eq_op(&mut self) {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result: Value = Value::Bool(arg_1.ne(&arg_2));

        self.push(result);
    }

    fn greater_than_op(&mut self) {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result: Value = Value::Bool(arg_1.gt(&arg_2));

        self.push(result);
    }

    fn less_than_op(&mut self) {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result: Value = Value::Bool(arg_1.lt(&arg_2));

        self.push(result);
    }

    fn logical_and_op(&mut self) {
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

    fn logical_or_op(&mut self) {
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

    fn get_local(&mut self, chunk: &Chunk) {
        let local_index: usize =
            chunk.get_instruction(self.instruction_pointer + 1).unwrap() as usize;
        if local_index > self.value_stack.len() - 1 {
            panic!("VM Error: Attempted getting a local that doesn't exist")
        }
        self.value_stack.push(self.value_stack[local_index].clone());
    }

    fn set_local(&mut self, chunk: &Chunk) {
        let local_index: usize =
            chunk.get_instruction(self.instruction_pointer + 1).unwrap() as usize;
        if local_index > self.value_stack.len() - 1 {
            panic!("VM Error: Attempted setting a local that doesn't exist")
        }
        self.value_stack[local_index] = self.value_stack.pop().unwrap();
    }

    // Utility Methods

    fn pop_value(&mut self) -> Value {
        self.value_stack.pop().unwrap()
    }

    fn pop_two_values(&mut self) -> (Value, Value) {
        let arg_2: Value = self.pop_value();
        let arg_1: Value = self.pop_value();

        (arg_1, arg_2)
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
    fn not_op_works() {
        test_unary_op(OpCode::LogicalNot, Value::Bool(true), Value::Bool(false));
        test_unary_op(OpCode::LogicalNot, Value::Bool(false), Value::Bool(true));
    }

    #[test]
    fn negate_op_works() {
        test_unary_op(OpCode::Negate, Value::Int(1), Value::Int(-1));
        test_unary_op(OpCode::Negate, Value::Int(-1), Value::Int(1));
        test_unary_op(OpCode::Negate, Value::Int(0), Value::Int(0));
    }

    #[test]
    fn add_op_works() {
        test_binary_op(OpCode::Add, (Value::Int(1), Value::Int(2)), Value::Int(3));
        test_binary_op(
            OpCode::Add,
            (
                Value::String("ch1".to_string()),
                Value::String("n3du".to_string()),
            ),
            Value::String("ch1n3du".to_string()),
        );
    }

    #[test]
    fn sub_op_works() {
        test_binary_op(OpCode::Sub, (Value::Int(1), Value::Int(2)), Value::Int(-1));
    }

    #[test]
    fn mul_op_works() {
        test_binary_op(OpCode::Mul, (Value::Int(3), Value::Int(2)), Value::Int(6));
    }

    #[test]
    fn div_op_works() {
        test_binary_op(OpCode::Div, (Value::Int(3), Value::Int(2)), Value::Int(1));
    }

    #[test]
    fn eq_op_works() {
        test_binary_op(
            OpCode::Eq,
            (Value::Int(3), Value::Int(3)),
            Value::Bool(true),
        );
        test_binary_op(
            OpCode::Eq,
            (Value::Int(3), Value::Int(1)),
            Value::Bool(false),
        );

        test_binary_op(
            OpCode::Eq,
            (
                Value::String("hello".to_string()),
                Value::String("hello".to_string()),
            ),
            Value::Bool(true),
        );
        test_binary_op(
            OpCode::Eq,
            (
                Value::String("hello".to_string()),
                Value::String("hi".to_string()),
            ),
            Value::Bool(false),
        );
    }

    #[test]
    fn not_eq_op_works() {
        test_binary_op(
            OpCode::NotEq,
            (Value::Int(3), Value::Int(3)),
            Value::Bool(false),
        );
        test_binary_op(
            OpCode::NotEq,
            (Value::Int(3), Value::Int(1)),
            Value::Bool(true),
        );

        test_binary_op(
            OpCode::NotEq,
            (
                Value::String("hello".to_string()),
                Value::String("hello".to_string()),
            ),
            Value::Bool(false),
        );
        test_binary_op(
            OpCode::NotEq,
            (
                Value::String("hello".to_string()),
                Value::String("hi".to_string()),
            ),
            Value::Bool(true),
        );
    }

    #[test]
    fn greater_than_op_works() {
        test_binary_op(
            OpCode::GreaterThan,
            (Value::Int(3), Value::Int(2)),
            Value::Bool(true),
        );
        test_binary_op(
            OpCode::GreaterThan,
            (Value::Int(2), Value::Int(3)),
            Value::Bool(false),
        );
    }

    #[test]
    fn less_than_op_works() {
        test_binary_op(
            OpCode::LessThan,
            (Value::Int(3), Value::Int(2)),
            Value::Bool(false),
        );
        test_binary_op(
            OpCode::LessThan,
            (Value::Int(2), Value::Int(3)),
            Value::Bool(true),
        );
    }

    #[test]
    fn get_local_works() {
        let mut chunky = Chunk::new();
        let one_index: usize = chunky.add_constant(Value::Int(1));
        let two_index: usize = chunky.add_constant(Value::Int(2));
        let three_index: usize = chunky.add_constant(Value::Int(3));

        // Put the 1, 2 and 3 on the stack
        chunky.write_opcode_with_args(OpCode::GetConstant, &[one_index as u8]);
        chunky.write_opcode_with_args(OpCode::GetConstant, &[two_index as u8]);
        chunky.write_opcode_with_args(OpCode::GetConstant, &[three_index as u8]);

        // Put 2 on the top of the stack.
        chunky.write_opcode_with_args(OpCode::GetLocal, &[two_index as u8]);

        // Return
        chunky.write_opcode(OpCode::Return);

        let mut vm = VM::new();
        let result: Value = vm.interpret(&chunky);
        assert_eq!(result, Value::Int(2))
    }

    #[test]
    fn set_local_works() {
        let mut chunky = Chunk::new();
        let one_index: usize = chunky.add_constant(Value::Int(1));
        let two_index: usize = chunky.add_constant(Value::Int(2));
        let three_index: usize = chunky.add_constant(Value::Int(3));

        // Put the 1, 2, 3 and 3 on the stack
        chunky.write_opcode_with_args(OpCode::GetConstant, &[one_index as u8]);
        chunky.write_opcode_with_args(OpCode::GetConstant, &[two_index as u8]);
        chunky.write_opcode_with_args(OpCode::GetConstant, &[three_index as u8]);
        chunky.write_opcode_with_args(OpCode::GetConstant, &[three_index as u8]);

        // Pop 3 from the top of the stack and set 2 to 3
        // Stack should look like: 1, 3, 3
        chunky.write_opcode_with_args(OpCode::SetLocal, &[two_index as u8]);

        // Return
        chunky.write_opcode(OpCode::Return);

        let mut vm = VM::new();
        let _result: Value = vm.interpret(&chunky);
        println!("VM {vm:?}");
        assert_eq!(vm.value_stack[two_index], Value::Int(3));
    }

    fn test_unary_op(op_code: OpCode, arg: Value, expected: Value) {
        let mut chunky = Chunk::new();
        let const_index: usize = chunky.add_constant(arg);

        // Put the arg on the stack and call 'op_code'
        chunky.write_opcode_with_args(OpCode::GetConstant, &[const_index as u8]);
        chunky.write_opcode(op_code);
        // println!("Chunk: {chunky:#?}");

        // Return
        chunky.write_opcode(OpCode::Return);

        let mut vm = VM::new();
        let result: Value = vm.interpret(&chunky);
        assert_eq!(result, expected);
    }

    fn test_binary_op(op_code: OpCode, (arg_1, arg_2): (Value, Value), expected: Value) {
        let mut chunky = Chunk::new();
        let const_index_1: usize = chunky.add_constant(arg_1);
        let const_index_2: usize = chunky.add_constant(arg_2);

        // Put the args on the stack and call 'op_code'
        chunky.write_opcode_with_args(OpCode::GetConstant, &[const_index_1 as u8]);
        chunky.write_opcode_with_args(OpCode::GetConstant, &[const_index_2 as u8]);
        chunky.write_opcode(op_code);
        // println!("Chunk: {chunky:#?}");

        // Return
        chunky.write_opcode(OpCode::Return);

        let mut vm = VM::new();
        let result: Value = vm.interpret(&chunky);
        assert_eq!(result, expected)
    }
}
