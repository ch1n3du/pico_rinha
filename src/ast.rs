use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Deserialize, Serialize)]
pub struct Program {
    pub name: String,
    pub expression: Expr,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
#[serde(tag = "kind")]
pub enum Expr {
    Int {
        value: i64,
        location: Location,
    },
    Var(Identifier),
    Unary {
        op: UnaryOp,
        rhs: Box<Expr>,
        location: Location,
    },
    Binary {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
        location: Location,
    },
    Let {
        name: Identifier,
        value: Box<Expr>,
        next: Option<Box<Expr>>,
        location: Location,
    },
    If {
        condition: Box<Expr>,
        then: Box<Expr>,
        otherwise: Box<Expr>,
        next: Option<Box<Expr>>,
        location: Location,
    },
    Print {
        value: Box<Expr>,
        next: Option<Box<Expr>>,
        location: Location,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
        next: Option<Box<Expr>>,
        location: Location,
    },
    Function {
        parameters: Vec<Identifier>,
        value: Box<Expr>,
        location: Location,
    },
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Identifier {
    pub text: String,
    pub location: Location,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Location {
    start: usize,
    end: usize,
    filename: String,
}

impl Default for Location {
    fn default() -> Self {
        Location {
            start: 0,
            end: 0,
            filename: "this is just for testing".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum BinaryOp {
    // Comparison
    Eq,
    NotEq,
    #[serde(rename = "Gt")]
    GreaterThan,
    #[serde(rename = "Lt")]
    LessThan,

    // Boolean
    And,
    Or,

    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BinaryOp::*;

        match self {
            Eq => write!(f, "=="),
            NotEq => write!(f, "!="),
            GreaterThan => write!(f, ">"),
            LessThan => write!(f, "<"),

            And => write!(f, "&&"),
            Or => write!(f, "||"),

            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum UnaryOp {
    Not,
    Negate,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Negate => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}
#[cfg(test)]
mod tests {
    use super::Program;

    const EXAMPLES: [&str; 3] = ["fib", "sum", "combination"];

    fn get_json_examples() -> Vec<(String, String)> {
        EXAMPLES
            .iter()
            .map(|file_name| {
                let path = format!("files/{file_name}.json");
                let Ok(file_content) = std::fs::read_to_string(&path) else {
                    panic!("Example file named '{path}' not found.")
                };

                (path, file_content)
            })
            .collect()
    }

    #[test]
    fn can_deserialize_examples() {
        for (file_name, file_content) in get_json_examples() {
            let deserialize_result: Result<Program, serde_json::Error> =
                serde_json::from_str(&file_content);
            println!("{deserialize_result:?}");
            assert!(deserialize_result.is_ok(), "Cannot parse '{file_name}'");
        }
    }

    #[test]
    fn roundtrip() {
        for (_, file_content) in get_json_examples() {
            let deserialize_result_1: Program = serde_json::from_str(&file_content).unwrap();

            let serialize_result: String = serde_json::to_string(&deserialize_result_1).unwrap();

            let deserialize_result_2: Program = serde_json::from_str(&serialize_result).unwrap();

            assert_eq!(deserialize_result_1, deserialize_result_2);
        }
    }
}
