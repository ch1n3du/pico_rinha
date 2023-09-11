use self::chunk::{Chunk, OpCode};

mod chunk;
mod compiler;

pub use chunk::{Function, Value};
pub use compiler::Compiler;

#[derive(Debug)]
pub struct VM {
    value_stack: Vec<Value>,
    frames: Vec<CallFrame>,
}

#[derive(Debug)]
pub struct CallFrame {
    function: Function,
    instruction_pointer: usize,
    slot_start: usize,
}

impl VM {
    pub fn new(main_function: Function) -> Self {
        VM {
            value_stack: Vec::with_capacity(256),
            frames: vec![CallFrame {
                function: main_function,
                instruction_pointer: 0,
                slot_start: 0,
            }],
        }
    }

    pub fn interpret(&mut self) -> Value {
        match self.interpret_() {
            Ok(value) => value,
            Err(VMError::Msg(msg)) => {
                println!("Program crashed with the following error:");
                println!("{msg}");

                println!("Stack Trace:");
                for (call_frame_index, call_frame) in self.frames.iter().enumerate().rev() {
                    println!("[{call_frame_index}] {}()", call_frame.function.name)
                }

                panic!()
            }
        }
    }

    /// Fetch, Decode, Execute loop
    fn interpret_(&mut self) -> VMResult<Value> {
        loop {
            // Fetch the current instruction
            let Some(raw_instruction): Option<u8> = self
                .frames
                .last()
                .unwrap()
                .function
                .chunk
                .get_instruction(self.get_instruction_pointer())
            else {
                panic!("VM Error: No more instructions to execute ")
            };

            // Decode the instruction
            let Some(op_code) = OpCode::from_u8(raw_instruction) else {
                panic!("VM Error: Invalid instruction '{raw_instruction}'.")
            };

            // TODO Remove
            // println!("{}", "-".repeat(30));
            // println!(
            //     "(IP = {}) Interpreting {op_code:?}\nVM: {self:#?}",
            //     self.get_instruction_pointer()
            // );
            // println!("{}", "-".repeat(30));
            // Execute the instruction
            match op_code {
                OpCode::Return => {
                    if self.frames.len() == 1 {
                        return Ok(self.pop_value());
                    } else {
                        self.frames.pop();
                        // TODO Might have to increment instruction pointer
                    }
                }
                OpCode::GetConstant => self.get_constant_op(),
                OpCode::Print => self.print_op(),
                OpCode::LogicalNot => self.logical_not_op()?,
                OpCode::Negate => self.negate_op()?,
                OpCode::Add => self.add_op()?,
                OpCode::Sub => self.sub_op()?,
                OpCode::Mul => self.mul_op()?,
                OpCode::Div => self.div_op()?,
                OpCode::Eq => self.eq_op(),
                OpCode::NotEq => self.not_eq_op(),
                OpCode::GreaterThan => self.greater_than_op(),
                OpCode::LessThan => self.less_than_op(),
                OpCode::LogicalAnd => self.logical_and_op()?,
                OpCode::LogicalOr => self.logical_or_op()?,
                OpCode::PushTrue => self.push(Value::Bool(true)),
                OpCode::PushFalse => self.push(Value::Bool(false)),
                OpCode::PushUnit => self.push(Value::Unit),
                OpCode::GetLocal => self.get_local_op()?,
                OpCode::SetLocal => self.set_local_op()?,
                OpCode::Pop => {
                    self.pop_value();
                }
                OpCode::PopN => self.popn_op(),
                OpCode::Jump => self.jump_op(),
                OpCode::JumpIfTrue => self.jump_if_true_op()?,
                OpCode::JumpIfFalse => self.jump_if_false_op()?,
                OpCode::Call => self.call_op()?,
            }

            match op_code {
                OpCode::Jump | OpCode::JumpIfTrue | OpCode::JumpIfFalse => (),
                _ => self.increment_instruction_pointer(1 + op_code.arity()),
            }
        }
    }

    fn get_constant_op(&mut self) {
        let constant_index: usize = self
            .get_chunk()
            .get_instruction(self.get_instruction_pointer() + 1)
            .unwrap() as usize;
        let constant: Value = self.get_chunk().get_constant(constant_index).unwrap();
        self.value_stack.push(constant);
    }

    fn print_op(&mut self) {
        let value: Value = self.pop_value();
        println!("{value}")
    }

    fn negate_op(&mut self) -> VMResult<()> {
        let value = self.value_stack.last_mut().unwrap();
        if let Value::Int(int) = value {
            *value = Value::Int(-(*int));
            Ok(())
        } else {
            Err(VMError::Msg(format!(
                "VM Error: Negation (unary '-') only works for Ints."
            )))
        }
    }

    fn logical_not_op(&mut self) -> VMResult<()> {
        let value = self.value_stack.last_mut().unwrap();
        if let Value::Bool(bool) = value {
            *value = Value::Bool(!(*bool));
            Ok(())
        } else {
            Err(VMError::Msg(format!(
                "VM Error: Logical not ('!') only works for booleans."
            )))
        }
    }

    fn add_op(&mut self) -> VMResult<()> {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();

        use Value::*;
        let result = match (arg_1, arg_2) {
            (Int(int_1), Int(int_2)) => Value::Int(int_1 + int_2),
            (String(string_1), String(string_2)) => Value::String(format!("{string_1}{string_2}")),
            (arg_1, arg_2) => {
                return Err(VMError::Msg(format!(
                    "VM Error: Addition is not supported between '{}' and '{}'",
                    arg_1.type_of(),
                    arg_2.type_of()
                )))
            }
        };

        self.push(result);
        Ok(())
    }

    fn sub_op(&mut self) -> VMResult<()> {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result = match (arg_1, arg_2) {
            (Value::Int(int_1), Value::Int(int_2)) => Value::Int(int_1 - int_2),
            (arg_1, arg_2) => {
                return Err(VMError::Msg(format!(
                    "VM Error: Subtraction is not supported between '{}' and '{}'",
                    arg_1.type_of(),
                    arg_2.type_of()
                )))
            }
        };

        self.push(result);
        Ok(())
    }

    fn mul_op(&mut self) -> VMResult<()> {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result = match (arg_1, arg_2) {
            (Value::Int(int_1), Value::Int(int_2)) => Value::Int(int_1 * int_2),
            (arg_1, arg_2) => {
                return Err(VMError::Msg(format!(
                    "VM Error: Multiplication is not supported between '{}' and '{}'",
                    arg_1.type_of(),
                    arg_2.type_of()
                )))
            }
        };

        self.push(result);
        Ok(())
    }

    fn div_op(&mut self) -> VMResult<()> {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result = match (arg_1, arg_2) {
            (Value::Int(int_1), Value::Int(int_2)) => Value::Int(int_1 / int_2),
            (arg_1, arg_2) => {
                return Err(VMError::Msg(format!(
                    "VM Error: Division is not supported between '{}' and '{}'",
                    arg_1.type_of(),
                    arg_2.type_of()
                )))
            }
        };

        self.push(result);
        Ok(())
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

    fn logical_and_op(&mut self) -> VMResult<()> {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result = match (arg_1, arg_2) {
            (Value::Bool(bool_1), Value::Bool(bool_2)) => Value::Bool(bool_1 && bool_2),
            (arg_1, arg_2) => {
                return Err(VMError::Msg(format!(
                    "VM Error: '<' is not supported between '{}' and '{}'",
                    arg_1.type_of(),
                    arg_2.type_of(),
                )))
            }
        };

        self.push(result);
        Ok(())
    }

    fn logical_or_op(&mut self) -> VMResult<()> {
        let (arg_1, arg_2): (Value, Value) = self.pop_two_values();
        let result = match (arg_1, arg_2) {
            (Value::Bool(bool_1), Value::Bool(bool_2)) => Value::Bool(bool_1 || bool_2),
            (arg_1, arg_2) => {
                return Err(VMError::Msg(format!(
                    "VM Error: '<' is not supported between '{}' and '{}'",
                    arg_1.type_of(),
                    arg_2.type_of()
                )))
            }
        };

        self.push(result);
        Ok(())
    }

    fn get_local_op(&mut self) -> VMResult<()> {
        let slot_start: usize = self.frames.last().unwrap().slot_start;
        let local_index: usize = slot_start
            + self
                .get_chunk()
                .get_instruction(self.get_instruction_pointer() + 1)
                .unwrap() as usize;
        if local_index > self.value_stack.len() - 1 {
            return Err(VMError::Msg(
                "VM Error: Attempted getting a local({local_index}) that doesn't exist".to_string(),
            ));
        }
        self.value_stack.push(self.value_stack[local_index].clone());

        Ok(())
    }

    fn set_local_op(&mut self) -> VMResult<()> {
        let slot_start: usize = self.frames.last().unwrap().slot_start;
        let local_index: usize = slot_start
            + self
                .get_chunk()
                .get_instruction(self.get_instruction_pointer() + 1)
                .unwrap() as usize;
        if local_index > self.value_stack.len() - 1 {
            return Err(VMError::Msg(
                "VM Error: Attempted setting a local({local_index}) that doesn't exist".to_string(),
            ));
        }
        self.value_stack[local_index] = self.value_stack.pop().unwrap();

        Ok(())
    }

    fn popn_op(&mut self) {
        let n: u8 = self
            .get_chunk()
            .get_instruction(self.get_instruction_pointer() + 1)
            .unwrap();
        self.value_stack
            .truncate(self.value_stack.len() - (n as usize))
    }

    fn jump_op(&mut self) {
        let instruction_index_bytes: [u8; 4] = self.get_chunk().instructions
            [self.get_instruction_pointer() + 1..self.get_instruction_pointer() + 5]
            .try_into()
            .unwrap();
        let instruction_index: usize = u32::from_be_bytes(instruction_index_bytes) as usize;
        self.set_instruction_pointer(instruction_index);
    }

    fn jump_if_true_op(&mut self) -> VMResult<()> {
        let Value::Bool(bool) = self.pop_value() else {
            return Err(VMError::Msg(
                "VM Error: 'JumpIfTrue' expects a boolean to be at the top of the stack."
                    .to_string(),
            ));
        };

        if bool == true {
            self.jump_op();
        } else {
            self.increment_instruction_pointer(1 + OpCode::JumpIfTrue.arity());
        }

        Ok(())
    }

    fn jump_if_false_op(&mut self) -> VMResult<()> {
        let Value::Bool(bool) = self.pop_value() else {
            return Err(VMError::Msg(
                "VM Error: 'JumpIfTrue' expects a boolean to be at the top of the stack."
                    .to_string(),
            ));
        };

        if bool == false {
            self.jump_op()
        } else {
            self.increment_instruction_pointer(1 + OpCode::JumpIfFalse.arity());
        }

        Ok(())
    }

    fn call_op(&mut self) -> VMResult<()> {
        let Value::Fn(function) = self.pop_value() else {
            return Err(VMError::Msg(
                "VM Error: Only functions are callable.".to_string(),
            ));
        };
        let slot_start: usize = self.value_stack.len();
        let call_frame: CallFrame = CallFrame {
            function: *function.clone(),
            instruction_pointer: 0,
            slot_start,
        };

        self.frames.push(call_frame);
        Ok(())
    }

    // Utility Methods

    fn get_instruction_pointer(&self) -> usize {
        self.frames.last().unwrap().instruction_pointer.clone()
    }

    fn set_instruction_pointer(&mut self, new_instruction_pointer: usize) {
        self.frames.last_mut().unwrap().instruction_pointer = new_instruction_pointer;
    }

    fn increment_instruction_pointer(&mut self, increment: usize) {
        self.frames.last_mut().unwrap().instruction_pointer += increment;
    }

    fn get_chunk(&self) -> &Chunk {
        &self.frames.last().unwrap().function.chunk
    }

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

pub enum VMError {
    Msg(String),
}

type VMResult<T> = Result<T, VMError>;

#[cfg(test)]
mod tests {
    use crate::vm::{chunk::Function, CallFrame};

    use super::{
        chunk::{self, Chunk, OpCode, Value},
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

        let mut vm = wrap_chunk(chunky);
        let result: Value = vm.interpret();
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

        let mut vm = wrap_chunk(chunky);
        let _result: Value = vm.interpret();
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

        let mut vm = wrap_chunk(chunky);
        let result: Value = vm.interpret();
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

        let mut vm = wrap_chunk(chunky);
        let result: Value = vm.interpret();
        assert_eq!(result, expected)
    }

    pub fn wrap_chunk(chunk: Chunk) -> VM {
        let mut function = Function::new_main();
        function.chunk = chunk;

        let mut vm = VM::new(function);
        vm
    }
}
