use std::{
    ffi::c_char,
    slice::{ChunksExact, ChunksMut},
};

use crate::ast::{BinaryOp, Expr, Identifier, Program, UnaryOp};

use super::chunk::{Chunk, OpCode, Value};

pub struct Compiler {
    locals: Vec<Local>,
    scope_depth: u32,
}

pub struct Local {
    name: String,
    depth: u32,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    pub fn compile(&mut self, program: Program) -> Chunk {
        let mut chunk: Chunk = Chunk::new();
        self.compile_expr(&program.expression, &mut chunk);
        chunk.write_opcode(OpCode::Return);
        chunk
    }

    fn compile_expr(&mut self, expr: &Expr, chunk: &mut Chunk) {
        match expr {
            Expr::Int { value, location: _ } => {
                let index = chunk.add_constant(Value::Int(*value)) as u8;
                chunk.write_opcode_with_args(OpCode::GetConstant, &[index]);
            }
            Expr::Bool { value, location } => {
                let index = chunk.add_constant(Value::Bool(*value)) as u8;
                chunk.write_opcode_with_args(OpCode::GetConstant, &[index]);
            }
            Expr::Unary {
                op,
                rhs,
                location: _,
            } => self.compile_unary_expr(chunk, op, rhs),
            Expr::Binary {
                lhs,
                op,
                rhs,
                location: _,
            } => self.compiler_binary_expr(chunk, op, lhs, rhs),
            Expr::Let {
                name,
                value,
                next,
                location: _,
            } => self.compile_let_expr(chunk, name.text.clone(), value, next),
            Expr::Var(Identifier { text, location: _ }) => self.compile_var_expr(chunk, &text),
            Expr::Print {
                value,
                next,
                location: _,
            } => self.compile_print_exr(chunk, value, next),
            Expr::If {
                condition,
                then,
                otherwise,
                next,
                location: _,
            } => self.compile_if_expr(chunk, condition, then, otherwise, next),
            Expr::Function {
                parameters,
                value,
                location: _,
            } => todo!(),
            Expr::Call {
                callee,
                arguments,
                next,
                location: _,
            } => todo!(),
        }
    }

    fn compile_unary_expr(&mut self, chunk: &mut Chunk, op: &UnaryOp, rhs: &Expr) {
        // Compile the right hand side
        self.compile_expr(rhs, chunk);
    }

    fn compiler_binary_expr(&mut self, chunk: &mut Chunk, op: &BinaryOp, lhs: &Expr, rhs: &Expr) {
        // Compile the left hand side leaving it at the top of the stack.
        self.compile_expr(lhs, chunk);
        // Compile the right hand side leaving it at the top of the stack.
        self.compile_expr(rhs, chunk);

        // Call the appropriate OpCode
        match op {
            BinaryOp::Add => chunk.write_opcode(OpCode::Add),
            BinaryOp::Sub => chunk.write_opcode(OpCode::Sub),
            BinaryOp::Mul => chunk.write_opcode(OpCode::Mul),
            BinaryOp::Div => chunk.write_opcode(OpCode::Div),

            BinaryOp::Eq => chunk.write_opcode(OpCode::Eq),
            BinaryOp::NotEq => chunk.write_opcode(OpCode::NotEq),
            BinaryOp::GreaterThan => chunk.write_opcode(OpCode::GreaterThan),
            BinaryOp::LessThan => chunk.write_opcode(OpCode::LessThan),

            BinaryOp::And => chunk.write_opcode(OpCode::LogicalAnd),
            BinaryOp::Or => chunk.write_opcode(OpCode::LogicalOr),
        };
    }

    fn compile_let_expr(
        &mut self,
        chunk: &mut Chunk,
        name: String,
        value: &Expr,
        next_expr: &Option<Box<Expr>>,
    ) {
        // Compile the value keeping it on the top of stack.
        self.compile_expr(value, chunk);
        // Create a local to keep track of it's position on the stack.
        self.locals.push(Local {
            name,
            depth: self.scope_depth,
        });

        // Compile the continuation
        if let Some(next_expr) = next_expr {
            self.compile_expr(next_expr, chunk)
        }
    }

    fn compile_var_expr(&mut self, chunk: &mut Chunk, name: &str) {
        let mut variable_local_index: Option<usize> = None;

        for (local_index, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                variable_local_index = Some(local_index);
                break;
            }
        }

        if let Some(local_index) = variable_local_index {
            chunk.write_opcode_with_args(OpCode::GetLocal, &[local_index as u8]);
        }
    }

    fn compile_print_exr(
        &mut self,
        chunk: &mut Chunk,
        value: &Expr,
        next_expr: &Option<Box<Expr>>,
    ) {
        // Compile the value to be printed leaving it at the top of the stack.
        self.compile_expr(value, chunk);
        chunk.write_opcode(OpCode::Print);

        // Compile the continuation
        if let Some(next_expr) = next_expr {
            self.compile_expr(next_expr, chunk)
        }
    }

    fn compile_if_expr(
        &mut self,
        chunk: &mut Chunk,
        condition: &Expr,
        then: &Expr,
        otherwise: &Option<Box<Expr>>,
        next: &Option<Box<Expr>>,
    ) {
        // Compile the condition leaving it at the top of the stack.
        self.compile_expr(condition, chunk);

        // Write a dummy jump instruction for the else block
        let jump_to_else_block_address_index: usize =
            chunk.write_opcode_with_args(OpCode::JumpIfFalse, 69_u32.to_be_bytes().as_slice()) + 1;

        // Compile the truthy block
        self.compile_expr(then, chunk);

        // Write a dummy jump instruction for the else block
        let jump_to_after_else_block_address_index: usize =
            chunk.write_opcode_with_args(OpCode::Jump, 69_u32.to_be_bytes().as_slice()) + 1;

        // Patch the jump_to_else_block address
        chunk.patch_address(
            chunk.instructions.len() as u32,
            jump_to_else_block_address_index,
        );

        if let Some(else_branh) = otherwise {
            self.compile_expr(&else_branh, chunk);
        }

        // Patch the jump_to_after_else_block address
        chunk.patch_address(
            chunk.instructions.len() as u32,
            jump_to_after_else_block_address_index,
        );

        if let Some(next_expression) = next {
            // If there's a continuation pop the result of the if/else block off of the stack.
            chunk.write_opcode(OpCode::Pop);
            self.compile_expr(&next_expression, chunk)
        }
    }

    // Utility Methods

    fn push_scope(&mut self) {
        self.scope_depth += 1
    }

    fn pop_scope(&mut self) {
        let mut previous_scope_end_index = self.locals.len();
        // Loop through the locals in reverse
        for i in (0..self.locals.len()).rev() {
            if self.locals[i].depth < self.scope_depth {
                previous_scope_end_index = i;
                break;
            }
        }
        // Drop all of the current scope's locals
        self.locals.truncate(previous_scope_end_index + 1);

        self.scope_depth -= 1
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{BinaryOp, Expr, Location, Program},
        vm::{
            chunk::{Chunk, OpCode, Value},
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

        let mut test_chunk: Chunk = Chunk::new();
        let mut compiler = Compiler::new();
        compiler.compile_expr(&expr, &mut test_chunk);

        assert_eq!(expected_chunk, test_chunk)
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

        let mut compiler = Compiler::new();
        let chunk = compiler.compile(Program {
            name: "Testing If/else".to_string(),
            expression: expr,
        });

        let result = VM::new().interpret(&chunk);
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

        let mut compiler = Compiler::new();
        let chunk = compiler.compile(Program {
            name: "Testing If/else".to_string(),
            expression: expr,
        });

        let result = VM::new().interpret(&chunk);
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

        let mut compiler = Compiler::new();
        let chunk = compiler.compile(Program {
            name: "Testing If/else".to_string(),
            expression: expr,
        });

        let result = VM::new().interpret(&chunk);
        assert_eq!(result, Value::Int(42));
    }
}
