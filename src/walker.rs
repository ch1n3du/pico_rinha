//! A tree walking interpret_exprer

use std::collections::HashMap;

use crate::ast::{BinaryOp, Expr, Identifier, Location, Program, UnaryOp};

/// A tree-walking interpret_exprer
pub struct Walker {
    environment: Environment,
}

impl Walker {
    pub fn new() -> Walker {
        Walker {
            environment: Environment::new(),
        }
    }

    pub fn interpret(&mut self, program: &Program) -> WalkerResult<Value> {
        self.interpret_expr(&program.expression)
    }

    pub fn interpret_expr(&mut self, expr: &Expr) -> WalkerResult<Value> {
        match expr {
            Expr::Int { value, .. } => Ok(Value::Int(*value)),
            Expr::Bool { value, .. } => Ok(Value::Bool(*value)),
            Expr::Unary { op, rhs, location } => {
                let rhs: Value = self.interpret_expr(&rhs)?;

                self.interpret_unary_expr(op.clone(), rhs, location.clone())
            }
            Expr::Binary {
                lhs,
                op,
                rhs,
                location,
            } => {
                let lhs: Value = self.interpret_expr(&lhs)?;
                let rhs: Value = self.interpret_expr(&rhs)?;

                self.interpret_binary_expr(op.clone(), lhs, rhs, location.clone())
            }
            Expr::Let {
                name,
                value,
                location: _,
                next,
            } => {
                let value: Value = self.interpret_expr(value)?;
                self.environment.create_variable(name.text.clone(), value);

                if let Some(next_expression) = next {
                    self.interpret_expr(next_expression)
                } else {
                    Ok(Value::Unit)
                }
            }
            Expr::Var(Identifier { text, location }) => {
                if let Some(value) = self.environment.get_variable(text) {
                    Ok(value)
                } else {
                    Err(WalkerErr::VarDoesNotExist {
                        name: text.to_string(),
                        location: location.clone(),
                    })
                }
            }
            Expr::If {
                condition,
                then,
                otherwise,
                location,
                next,
            } => {
                // interpret_expr the condition
                let condition = match self.interpret_expr(&condition)? {
                    Value::Bool(b) => b,
                    _ => {
                        return Err(WalkerErr::Msg {
                            msg: "If/Else condition must be a boolean expression.".to_string(),
                            location: location.clone(),
                        })
                    }
                };

                // interpret_expr the branches
                let result: Value = if condition {
                    self.interpret_expr(then)?
                } else {
                    if let Some(else_branch) = otherwise {
                        self.interpret_expr(&else_branch)?
                    } else {
                        Value::Unit
                    }
                };

                if let Some(next_expression) = next {
                    self.interpret_expr(&next_expression)
                } else {
                    Ok(result)
                }
            }
            Expr::Print {
                value,
                next,
                location: _,
            } => {
                let value: Value = self.interpret_expr(&value)?;
                println!("{value}");

                if let Some(next_expression) = next {
                    self.interpret_expr(&next_expression)
                } else {
                    Ok(Value::Unit)
                }
            }
            Expr::Function {
                parameters,
                value,
                location: _,
            } => {
                let arguments: Vec<String> = parameters
                    .into_iter()
                    .map(|Identifier { text, .. }| text.to_string())
                    .collect();
                let function: Value = Value::Fn {
                    arguments,
                    body: value.as_ref().clone(),
                };

                Ok(function)
            }
            Expr::Call {
                callee,
                arguments: call_arguments,
                location,
                next,
            } => {
                // interpret_expr the function call arguments.
                let mut argument_values: Vec<Value> = Vec::new();
                for argument in call_arguments {
                    argument_values.push(self.interpret_expr(argument)?);
                }

                let Value::Fn {
                    arguments: argument_names,
                    body: function_body,
                } = self.interpret_expr(&callee)?
                else {
                    return Err(WalkerErr::Msg {
                        msg: "Attempted to call a non-function".to_string(),
                        location: location.clone(),
                    });
                };

                // Check that the function arity matches
                if argument_names.len() != argument_values.len() {
                    return Err(WalkerErr::IncorrectArgCount {
                        expected: argument_names.len(),
                        got: argument_values.len(),
                        location: location.clone(),
                    });
                }

                // Create a new scope for the function call
                self.environment.push_new_scope();

                // Insert the arguments in the new scope
                for (argument_name, argument_value) in
                    argument_names.into_iter().zip(argument_values.into_iter())
                {
                    self.environment
                        .create_variable(argument_name, argument_value);
                }

                // interpret_expr the function body
                let return_value: Value = self.interpret_expr(&function_body)?;

                // Pop from the call stack
                self.environment.pop_scope();

                if let Some(next_expression) = next {
                    self.interpret_expr(&next_expression)
                } else {
                    Ok(return_value)
                }
            }
        }
    }

    fn interpret_unary_expr(
        &self,
        op: UnaryOp,
        rhs: Value,
        location: Location,
    ) -> WalkerResult<Value> {
        let result: Value = match (op, rhs) {
            (UnaryOp::Negate, Value::Int(int)) => Value::Int(-int),
            (UnaryOp::Not, Value::Bool(booly)) => Value::Bool(booly),
            (op, rhs) => {
                return Err(WalkerErr::Msg {
                    msg: format!(
                        "The '{op}' unary operator doesn't work for values of type '{}'",
                        rhs.type_of()
                    ),
                    location: location,
                })
            }
        };

        Ok(result)
    }

    fn interpret_binary_expr(
        &self,
        op: BinaryOp,
        lhs: Value,
        rhs: Value,
        location: Location,
    ) -> WalkerResult<Value> {
        let result: Value = match (op, lhs, rhs) {
            // Arithmetic Operators
            (BinaryOp::Add, Value::Int(int_1), Value::Int(int_2)) => Value::Int(int_1 + int_2),
            (BinaryOp::Sub, Value::Int(int_1), Value::Int(int_2)) => Value::Int(int_1 - int_2),
            (BinaryOp::Mul, Value::Int(int_1), Value::Int(int_2)) => Value::Int(int_1 * int_2),
            (BinaryOp::Div, Value::Int(int_1), Value::Int(int_2)) => Value::Int(int_1 / int_2),

            // String concatenation
            (BinaryOp::Add, Value::String(string_1), Value::Int(string_2)) => {
                Value::String(format!("{string_1}{string_2}"))
            }

            // Boolean Operators
            (BinaryOp::And, Value::Bool(bool_1), Value::Bool(bool_2)) => {
                Value::Bool(bool_1 && bool_2)
            }
            (BinaryOp::Or, Value::Bool(bool_1), Value::Bool(bool_2)) => {
                Value::Bool(bool_1 || bool_2)
            }

            // Comparison Operators
            (BinaryOp::Eq, value_1, value_2) => Value::Bool(value_1 == value_2),
            (BinaryOp::NotEq, value_1, value_2) => Value::Bool(value_1 != value_2),
            (BinaryOp::GreaterThan, Value::Int(int_1), Value::Int(int_2)) => {
                Value::Bool(int_1 > int_2)
            }
            (BinaryOp::LessThan, Value::Int(int_1), Value::Int(int_2)) => {
                Value::Bool(int_1 < int_2)
            }

            (op, lhs, rhs) => {
                return Err(WalkerErr::Msg {
                    msg: format!(
                    "The '{op}' binary operator doesn't work between values of type '{}' and '{}'",
                    lhs.type_of(),
                    rhs.type_of()
                ),
                    location: location,
                })
            }
        };

        Ok(result)
    }
}

/// Represents a runtime value.
#[derive(PartialEq, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    String(String),
    Fn { arguments: Vec<String>, body: Expr },
    Unit,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "<{{unit}}>"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Int(i) => write!(f, "{i}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Fn { .. } => write!(f, "<{{Function}}>"),
        }
    }
}

impl Value {
    fn type_of(&self) -> String {
        match self {
            Value::Bool(_) => "Bool".to_string(),
            Value::Int(_) => "Int".to_string(),
            Value::String(_) => "String".to_string(),
            Value::Fn { .. } => "<{{Function}}>".to_string(),
            Value::Unit => "Unit".to_string(),
        }
    }
}

struct Environment {
    scopes: Vec<HashMap<String, Value>>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            scopes: vec![HashMap::new()],
        }
    }

    fn push_new_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        } else {
            panic!("Cannot pop base scope.");
        }
    }

    fn create_variable(&mut self, name: String, value: Value) {
        self.scopes.last_mut().unwrap().insert(name, value);
    }

    fn get_variable(&mut self, name: &str) -> Option<Value> {
        // Loop in reverse to get the latest definition
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }
        None
    }
}

#[derive(Debug)]
pub enum WalkerErr {
    VarDoesNotExist {
        name: String,
        location: Location,
    },
    IncorrectArgCount {
        expected: usize,
        got: usize,
        location: Location,
    },
    Msg {
        msg: String,
        location: Location,
    },
}

type WalkerResult<T> = Result<T, WalkerErr>;
