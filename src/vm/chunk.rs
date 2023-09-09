use core::panic;

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn get_instruction(&self, index: usize) -> Option<u8> {
        self.code.get(index).map(|byte| *byte)
    }

    pub fn get_constant(&self, index: usize) -> Option<Value> {
        self.constants.get(index).map(|value| value.clone())
    }

    pub fn write_opcode(&mut self, op: OpCode) {
        self.write_opcode_with_args(op, &[])
    }

    pub fn write_opcode_with_args(&mut self, op: OpCode, arguments: &[u8]) {
        if arguments.len() != op.arity() {
            panic!(
                "Expected {} operands for the {:?} OpCode",
                arguments.len(),
                op
            );
        }
        self.code.push(op as u8);
        self.code.extend_from_slice(arguments)
    }

    /// Adds a constant and returns it's index
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}

#[derive(Debug)]
#[repr(u8)]
pub enum OpCode {
    Return = 0,

    /// Reads the next byte as the index of the constant
    GetConstant,
    // Unary Operands
    Negate,
    LogicalNot,

    // Binary Operands:
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    // Comparison Operands
    Eq,
    NotEq,
    GreaterThan,
    LessThan,
    // Boolean Operands
    LogicalAnd,
    LogicalOr,

    // Constant value OpCodes
    PushUnit,
    PushTrue,
    PushFalse,
}

impl OpCode {
    /// Returns the number of bytes following this OpCode that are it's arguments
    pub fn arity(&self) -> usize {
        match self {
            OpCode::GetConstant => 1,
            // OpCode::Pop
            // OpCode::PopN
            _ => 0,
        }
    }

    pub fn from_u8(byte: u8) -> Option<OpCode> {
        use OpCode::*;
        let op_code: OpCode = match byte {
            b if b == Return as u8 => Return,
            b if b == GetConstant as u8 => GetConstant,

            // Unary Operands
            b if b == Negate as u8 => Negate,
            b if b == LogicalNot as u8 => LogicalNot,

            // Binary Operands:
            // Arithmetic
            b if b == Add as u8 => Add,
            b if b == Sub as u8 => Sub,
            b if b == Mul as u8 => Mul,
            b if b == Div as u8 => Div,
            // Comparison Operands
            b if b == Eq as u8 => Eq,
            b if b == NotEq as u8 => NotEq,
            b if b == GreaterThan as u8 => GreaterThan,
            b if b == LessThan as u8 => LessThan,
            // Boolean Operands
            b if b == LogicalAnd as u8 => LogicalAnd,
            b if b == LogicalOr as u8 => LogicalOr,

            // Constant value OpCodes
            b if b == PushUnit as u8 => PushUnit,
            b if b == PushTrue as u8 => PushTrue,
            b if b == PushFalse as u8 => PushFalse,

            _ => return None,
        };
        Some(op_code)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Clone)]
pub enum Value {
    Int(i64),
    // TODO Find a way to have more compact strings
    String(String),
    Bool(bool),
    Unit,
}

impl Value {
    pub fn type_of(&self) -> String {
        match self {
            Value::Bool(_) => "Bool".to_string(),
            Value::Int(_) => "Int".to_string(),
            Value::String(_) => "String".to_string(),
            // Value::Fn { .. } => "<{{Function}}>".to_string(),
            Value::Unit => "Unit".to_string(),
        }
    }
}
