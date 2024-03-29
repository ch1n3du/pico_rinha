use crate::ast::{BinaryOp, Expr, Identifier, Program, UnaryOp};

use super::chunk::{Function, OpCode, Value};

/// Contains the context required while compiling a program
#[derive(Debug)]
pub struct Compiler {
    /// Represents the main function of the program and will be returned once `self.compile` is called.
    function: Function,
    /// Keeps track of local variables that will be on the stack at runtime to allow, them be referenced as stack indexes in the bytecode.
    locals: Vec<Local>,
    /// Keeps track of the current scope being compiled to allow removing the local variables of a scope from the stack
    /// once their parent scope ends.
    scope_depth: u32,
    /// Helps generate unique IDs for functions
    next_function_id: u32,
}

#[derive(Debug)]
pub struct Local {
    name: String,
    depth: u32,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            function: Function::new_main(),
            locals: Vec::new(),
            scope_depth: 0,
            next_function_id: 0,
        }
    }

    /// Creates a new compiler to compile a user-defined function.
    fn new_non_main(name: String, arguments: Vec<String>) -> Compiler {
        Compiler {
            function: Function::new(name.to_string(), arguments),
            locals: Vec::new(),
            scope_depth: 0,
            next_function_id: 0,
        }
    }

    /// Compiles a program into a `Function` that can be put in a `CallFrame` and run by a VM.
    pub fn compile(mut self, program: Program) -> Function {
        self.compile_expr(&program.expression);
        self.function.chunk.write_opcode(OpCode::Return);

        self.function
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Int { value, location: _ } => {
                let index = self.function.chunk.add_constant(Value::Int(*value)) as u8;
                self.function
                    .chunk
                    .write_opcode_with_args(OpCode::GetConstant, &[index]);
            }
            Expr::Bool { value, location: _ } => {
                let index = self.function.chunk.add_constant(Value::Bool(*value)) as u8;
                self.function
                    .chunk
                    .write_opcode_with_args(OpCode::GetConstant, &[index]);
            }
            Expr::Unary {
                op,
                rhs,
                location: _,
            } => self.compile_unary_expr(op, rhs),
            Expr::Binary {
                lhs,
                op,
                rhs,
                location: _,
            } => self.compiler_binary_expr(op, lhs, rhs),
            Expr::Let {
                name,
                value,
                next,
                location: _,
            } => self.compile_let_expr(name.text.clone(), value, next),
            Expr::Var(Identifier { text, location: _ }) => self.compile_var_expr(&text),
            Expr::Print {
                value,
                next,
                location: _,
            } => self.compile_print_exr(value, next),
            Expr::If {
                condition,
                then,
                otherwise,
                next,
                location: _,
            } => self.compile_if_expr(condition, then, otherwise, next),
            Expr::Function {
                parameters,
                value,
                location: _,
            } => self.compile_function_expr(parameters, value),
            Expr::Call {
                callee,
                arguments,
                next,
                location: _,
            } => {
                self.compile_call_expr(callee, arguments, next);
            }
        }
    }

    fn compile_unary_expr(&mut self, op: &UnaryOp, rhs: &Expr) {
        // Compile the right hand side leaving it at the top of the stack.
        self.compile_expr(rhs);

        // Call the appropriate OpCode with the right hand side on top of the stack as it's argument.
        match op {
            UnaryOp::Negate => self.function.chunk.write_opcode(OpCode::Negate),
            UnaryOp::Not => self.function.chunk.write_opcode(OpCode::LogicalNot),
        };
    }

    fn compiler_binary_expr(&mut self, op: &BinaryOp, lhs: &Expr, rhs: &Expr) {
        // Compile the left hand side leaving it at the top of the stack.
        self.compile_expr(lhs);
        // Compile the right hand side leaving it at the top of the left hand side on stack.
        self.compile_expr(rhs);

        // Call the appropriate OpCode with right hand side on the top of the stack and the left hand
        // directly below that.
        match op {
            BinaryOp::Add => self.function.chunk.write_opcode(OpCode::Add),
            BinaryOp::Sub => self.function.chunk.write_opcode(OpCode::Sub),
            BinaryOp::Mul => self.function.chunk.write_opcode(OpCode::Mul),
            BinaryOp::Div => self.function.chunk.write_opcode(OpCode::Div),

            BinaryOp::Eq => self.function.chunk.write_opcode(OpCode::Eq),
            BinaryOp::NotEq => self.function.chunk.write_opcode(OpCode::NotEq),
            BinaryOp::GreaterThan => self.function.chunk.write_opcode(OpCode::GreaterThan),
            BinaryOp::LessThan => self.function.chunk.write_opcode(OpCode::LessThan),

            BinaryOp::And => self.function.chunk.write_opcode(OpCode::LogicalAnd),
            BinaryOp::Or => self.function.chunk.write_opcode(OpCode::LogicalOr),
        };
    }

    fn compile_let_expr(&mut self, name: String, value: &Expr, next_expr: &Option<Box<Expr>>) {
        // Create a local to keep track of the variable's position on the stack.
        self.locals.push(Local {
            name,
            depth: self.scope_depth,
        });
        // Compile the expression being assigned to the variable keeping it on the top of stack.
        self.compile_expr(value);

        // Compile the continuation
        if let Some(next_expr) = next_expr {
            self.compile_expr(next_expr)
        }
    }

    fn compile_var_expr(&mut self, name: &str) {
        let mut variable_local_index: Option<usize> = None;

        for (local_index, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                variable_local_index = Some(local_index);
                break;
            }
        }

        if let Some(local_index) = variable_local_index {
            self.function
                .chunk
                .write_opcode_with_args(OpCode::GetLocal, &[local_index as u8]);
        } else {
            panic!(
                "Variable '{name}' doesn't exist, locals: {:?}.",
                self.locals
            );
        }
    }

    fn compile_print_exr(&mut self, value: &Expr, next_expr: &Option<Box<Expr>>) {
        // Compile the value to be printed leaving it at the top of the stack.
        self.compile_expr(value);
        // Call the print OpCode popping the value we put on top of the stack and printing it.
        self.function.chunk.write_opcode(OpCode::Print);

        // Compile the continuation
        if let Some(next_expr) = next_expr {
            self.compile_expr(next_expr)
        }
    }

    fn compile_if_expr(
        &mut self,
        condition: &Expr,
        then: &Expr,
        otherwise: &Option<Box<Expr>>,
        next: &Option<Box<Expr>>,
    ) {
        // Push a dummy value to the stack that will hold the eventual result value of the if/else
        // expression.
        self.function.chunk.write_opcode(OpCode::PushUnit);
        self.locals.push(Local {
            name: "~~if_else_value~~".to_string(),
            depth: self.scope_depth,
        });
        let if_else_value_index: u8 = (self.locals.len() - 1) as u8;

        // Compile the condition leaving it's result on the top of the stack.
        self.compile_expr(condition);

        // Write a dummy jump instruction, to jump to the else block,
        // we'll patch this later (in P1) once we finish compiling the if branch
        // and know where the instructions for the else block will start.
        let jump_to_else_block_address_index: usize = self
            .function
            .chunk
            .write_opcode_with_args(OpCode::JumpIfFalse, 69_u32.to_be_bytes().as_slice())
            + 1;

        // Compile the if branch
        self.push_scope();
        self.compile_expr(then);

        // Store the final value at the top of the stack in the if expression
        // in the if_else_value_index
        self.locals.pop();
        self.function.chunk.write_opcode_with_args(
            OpCode::SetLocal,
            if_else_value_index.to_be_bytes().as_slice(),
        );

        // Pop the if branch locals
        self.pop_scope();

        // Write a dummy jump instruction, to jump to after the else block,
        // we'll patch this later (in P2) once we finish compiling the else branch
        // and know where the instructions after the else block will start.
        let jump_to_after_else_block_address_index: usize = self
            .function
            .chunk
            .write_opcode_with_args(OpCode::Jump, 69_u32.to_be_bytes().as_slice())
            + 1;

        // (P1) Patch the instruction to jump to the else block.
        self.function.chunk.patch_address(
            self.function.chunk.instructions.len() as u32,
            jump_to_else_block_address_index,
        );

        // If there's an else branch compile it.
        if let Some(else_branh) = otherwise {
            self.push_scope();
            self.compile_expr(&else_branh);

            // Pop the value at the top of the stack and store it in the if/else value.
            self.locals.pop();
            self.function.chunk.write_opcode_with_args(
                OpCode::SetLocal,
                if_else_value_index.to_be_bytes().as_slice(),
            );

            // Pop the else branch locals
            self.pop_scope();
        }

        // (P2) Patch the instruction to jump to the else block.
        self.function.chunk.patch_address(
            self.function.chunk.instructions.len() as u32,
            jump_to_after_else_block_address_index,
        );

        if let Some(next_expression) = next {
            // If there's a continuation pop the result of the if/else block off of the stack.
            self.function.chunk.write_opcode(OpCode::Pop);
            self.compile_expr(&next_expression);
        }
    }

    fn compile_function_expr(&mut self, parameters: &[Identifier], value: &Expr) {
        let parameter_names: Vec<String> = parameters
            .into_iter()
            .map(|identifier| identifier.text.clone())
            .collect();

        // Create a unique ID for the function
        let function_id = format!("<fn anon_{} is shy uwu>", self.new_function_id());

        // Check if the function's arguments are within the allowed number.
        if parameters.len() > u8::MAX as usize {
            panic!(
                "Compiler Error: Functions can only have {} arguments",
                u8::MAX
            );
        }

        // Create a new compiler to compile the function
        let mut function_compiler = Compiler::new_non_main(function_id, parameter_names.clone());

        // Push all the function's parameters as local variables
        for parameter_name in parameter_names.iter().cloned() {
            function_compiler.locals.push(Local {
                name: parameter_name,
                depth: self.scope_depth,
            })
        }

        // Compile the function and wrap it in a Value.
        let compiled_function: Function = function_compiler.compile(Program {
            name: "".to_string(),
            expression: value.clone(),
        });
        let function_object: Value = Value::Fn(Box::new(compiled_function));

        let function_object_index = self.function.chunk.add_constant(function_object);
        self.function
            .chunk
            .write_opcode_with_args(OpCode::GetConstant, &[function_object_index as u8]);
    }

    fn compile_call_expr(&mut self, callee: &Expr, arguments: &[Expr], next: &Option<Box<Expr>>) {
        // Push a dummy return value
        self.function.chunk.write_opcode(OpCode::PushUnit);
        self.locals.push(Local {
            name: "~~return_address~~".to_string(),
            depth: self.scope_depth,
        });
        let return_value_index: u8 = (self.locals.len() - 1) as u8;

        // Create a new scope
        self.push_scope();

        // Push the arguments to the stack.
        for argument in arguments.iter() {
            self.compile_expr(argument);
        }

        // Compile the callee
        self.compile_expr(callee);

        // Call the callee
        self.function.chunk.write_opcode(OpCode::Call);

        // Pop the value and the top of the stack and store it in the return address.
        self.locals.pop();
        self.function.chunk.write_opcode_with_args(
            OpCode::SetLocal,
            return_value_index.to_be_bytes().as_slice(),
        );

        // Pop the function arguments and other locals
        self.pop_scope();

        if let Some(next_expression) = next {
            // If there's a continuation pop the result of the if/else block off of the stack.
            self.function.chunk.write_opcode(OpCode::Pop);
            self.compile_expr(&next_expression)
        }
    }

    // Utility Methods

    /// Creates a new scope.
    fn push_scope(&mut self) {
        self.scope_depth += 1;
    }

    /// Removes the previous scope.
    fn pop_scope(&mut self) {
        let mut current_scope_local_count = 0;
        // Loop through the locals in reverse
        for local_index in (0..self.locals.len()).rev() {
            if self.locals[local_index].depth < self.scope_depth {
                let previous_scope_locals_end_index = local_index;
                current_scope_local_count =
                    self.locals.len() - (previous_scope_locals_end_index + 1);
                break;
            }
        }

        // Drop all of the current scope's locals and decrement the scope depth
        self.function
            .chunk
            .write_opcode_with_args(OpCode::PopN, &[current_scope_local_count as u8]);
        self.locals
            .truncate(self.locals.len() - current_scope_local_count);
        self.scope_depth -= 1;
    }

    fn new_function_id(&mut self) -> u32 {
        let id = self.next_function_id;
        self.next_function_id += 1;
        id
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{BinaryOp, Expr, Identifier, Location, Program},
        vm::{
            chunk::{Chunk, Function, OpCode, Value},
            VM,
        },
    };

    use super::Compiler;

    #[test]
    fn can_compile_binary_expressions() {
        let mut expected_chunk: Chunk = Chunk::new();
        let const_index_1 = expected_chunk.add_constant(Value::Int(1)) as u8;
        let const_index_2 = expected_chunk.add_constant(Value::Int(1)) as u8;
        expected_chunk.write_opcode_with_args(OpCode::GetConstant, &[const_index_1]);
        expected_chunk.write_opcode_with_args(OpCode::GetConstant, &[const_index_2]);
        expected_chunk.write_opcode(OpCode::Add);
        expected_chunk.write_opcode(OpCode::Return);

        let expr: Expr = Expr::Binary {
            lhs: Box::new(Expr::Int {
                value: 1,
                location: Location::default(),
            }),
            op: BinaryOp::Add,
            rhs: Box::new(Expr::Int {
                value: 1,
                location: Location::default(),
            }),
            location: Location::default(),
        };

        let function = Compiler::new().compile(Program {
            name: "Testing binary expr".to_string(),
            expression: expr,
        });

        assert_eq!(
            expected_chunk, function.chunk,
            "\n\nExpected this Chunk:\n{expected_chunk}\n\nBut Got this chunk:\n{}",
            function.chunk
        )
    }

    #[test]
    fn can_compile_if_expressions() {
        let expr: Expr = Expr::If {
            condition: Box::new(Expr::Bool {
                value: true,
                location: Location::default(),
            }),
            then: Box::new(Expr::Int {
                value: 0,
                location: Location::default(),
            }),
            otherwise: None,
            next: None,
            location: Location::default(),
        };

        let mut vm: VM = compile_expr_to_vm("Testing If expressions", expr);
        let result = vm.interpret().unwrap();
        assert_eq!(result, Value::Int(0));
    }

    #[test]
    fn can_compile_if_else_expressions() {
        let expr: Expr = Expr::If {
            condition: Box::new(Expr::Bool {
                value: false,
                location: Location::default(),
            }),
            then: Box::new(Expr::Int {
                value: 0,
                location: Location::default(),
            }),
            otherwise: Some(Box::new(Expr::Int {
                value: 1,
                location: Location::default(),
            })),
            next: None,
            location: Location::default(),
        };

        let mut vm = compile_expr_to_vm("Testing If/Else expressions", expr);
        let result = vm.interpret().unwrap();
        assert_eq!(result, Value::Int(1));
    }

    #[test]
    fn can_compile_if_else_expressions_with_continuations() {
        let expr: Expr = Expr::If {
            condition: Box::new(Expr::Bool {
                value: false,
                location: Location::default(),
            }),
            then: Box::new(Expr::Int {
                value: 0,
                location: Location::default(),
            }),
            otherwise: Some(Box::new(Expr::Int {
                value: 1,
                location: Location::default(),
            })),
            next: Some(Box::new(Expr::Int {
                value: 42,
                location: Location::default(),
            })),
            location: Location::default(),
        };

        let mut vm = compile_expr_to_vm("Testing If/Else expressions", expr);
        let result = vm.interpret().unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn can_compile_plus_one_function() {
        let plus_one_function_value: Expr = Expr::Function {
            parameters: vec![Identifier::new_dummy("number")],
            value: Box::new(Expr::Binary {
                lhs: Box::new(Expr::Var(Identifier::new_dummy("number"))),
                op: BinaryOp::Add,
                rhs: Box::new(Expr::Int {
                    value: 1,
                    location: Location::default(),
                }),
                location: Location::default(),
            }),
            location: Location::default(),
        };

        let plus_one_call: Expr = Expr::Call {
            callee: Box::new(Expr::Var(Identifier::new_dummy("plus_one"))),
            arguments: vec![Expr::Int {
                value: 2,
                location: Location::default(),
            }],
            next: None,
            location: Location::default(),
        };

        let complet_expr: Expr = Expr::Let {
            name: Identifier::new_dummy("plus_one"),
            value: Box::new(plus_one_function_value),
            next: Some(Box::new(plus_one_call)),
            location: Location::default(),
        };

        let mut vm = compile_expr_to_vm("Testing function definitions and calls", complet_expr);
        let result = vm.interpret().unwrap();
        // println!("Main Function chunk:\n{}", vm.frames[0].function.chunk);
        assert_eq!(result, Value::Int(3));
    }

    #[test]
    fn can_compile_fib_function() {
        let fib_recursive_call: Expr = Expr::Binary {
            lhs: Box::new(Expr::Call {
                callee: Box::new(Expr::Var(Identifier::new_dummy("fib"))),
                arguments: vec![Expr::Binary {
                    lhs: Box::new(Expr::Var(Identifier::new_dummy("n"))),
                    op: BinaryOp::Sub,
                    rhs: Box::new(Expr::Int {
                        value: 1,
                        location: Location::default(),
                    }),
                    location: Location::default(),
                }],
                next: None,
                location: Location::default(),
            }),
            op: BinaryOp::Add,
            rhs: Box::new(Expr::Call {
                callee: Box::new(Expr::Var(Identifier::new_dummy("fib"))),
                arguments: vec![Expr::Binary {
                    lhs: Box::new(Expr::Var(Identifier::new_dummy("n"))),
                    op: BinaryOp::Sub,
                    rhs: Box::new(Expr::Int {
                        value: 2,
                        location: Location::default(),
                    }),
                    location: Location::default(),
                }],
                next: None,
                location: Location::default(),
            }),
            location: Location::default(),
        };

        let fib_if_else: Expr = Expr::If {
            condition: Box::new(Expr::Binary {
                lhs: Box::new(Expr::Var(Identifier::new_dummy("n"))),
                op: BinaryOp::LessThan,
                rhs: Box::new(Expr::Int {
                    value: 2,
                    location: Location::default(),
                }),
                location: Location::default(),
            }),
            then: Box::new(Expr::Int {
                value: 1,
                location: Location::default(),
            }),
            otherwise: Some(Box::new(fib_recursive_call)),
            next: None,
            location: Location::default(),
        };

        let fib_function_value: Expr = Expr::Function {
            parameters: vec![Identifier::new_dummy("n")],
            value: Box::new(fib_if_else),
            location: Location::default(),
        };

        let fib_call: Expr = Expr::Call {
            callee: Box::new(Expr::Var(Identifier::new_dummy("fib"))),
            arguments: vec![Expr::Int {
                value: 2,
                location: Location::default(),
            }],
            next: None,
            location: Location::default(),
        };

        let complete_expr: Expr = Expr::Let {
            name: Identifier::new_dummy("plus_one"),
            value: Box::new(fib_function_value),
            next: Some(Box::new(fib_call)),
            location: Location::default(),
        };

        let mut vm = compile_expr_to_vm("Testing recursive functions", complete_expr);
        let result = vm.interpret().unwrap();
        assert_eq!(result, Value::Int(2));
    }

    pub fn compile_expr_to_vm(title: &str, expr: Expr) -> VM {
        let mut main_function: Function = Compiler::new().compile(Program {
            name: title.to_string(),
            expression: expr,
        });

        VM::new(main_function)
    }
}
