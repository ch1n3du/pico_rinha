use core::panic;

#[derive(Debug, PartialEq)]
pub struct Chunk {
    pub instructions: Vec<u8>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            instructions: Vec::new(),
            constants: Vec::new(),
        }
    }

    /// Returns a byte from the chunk
    pub fn get_instruction(&self, index: usize) -> Option<u8> {
        self.instructions.get(index).map(|byte| *byte)
    }

    /// Get a Value stored in this chunk.
    pub fn get_constant(&self, index: usize) -> Option<Value> {
        self.constants.get(index).map(|value| value.clone())
    }

    /// Writes an OpCode to this chunk's instructions
    pub fn write_opcode(&mut self, op: OpCode) -> usize {
        self.write_opcode_with_args(op, &[])
    }

    // TODO If no other multi-argument OpCodes make arguments a u8
    /// Writes an OpCode and it's arguments to this chunk's instructions
    pub fn write_opcode_with_args(&mut self, op: OpCode, arguments: &[u8]) -> usize {
        if arguments.len() != op.arity() {
            panic!(
                "Expected {} operands for the {:?} OpCode",
                arguments.len(),
                op
            );
        }
        self.instructions.push(op as u8);
        let instruction_index: usize = self.instructions.len() - 1;
        self.instructions.extend_from_slice(arguments);

        instruction_index
    }

    pub fn patch_address(&mut self, address: u32, index: usize) {
        println!("About to patch {index}");
        for (i, byte) in address.to_be_bytes().into_iter().enumerate() {
            println!(
                "Index: {}, Instruction Lenght: {}",
                index + i,
                self.instructions.len()
            );
            self.instructions[index + i] = byte;
        }
    }

    /// Adds a constant and returns it's index
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}

impl std::fmt::Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let separator = "=".repeat(50);
        writeln!(f, "{}", separator)?;
        writeln!(f, "INSTRUCTIONS:")?;
        writeln!(f, "{}", separator)?;
        let mut instruction_pointer: usize = 0;
        let mut instruction_counter: usize = 0;
        while instruction_pointer < self.instructions.len() {
            let op_code: OpCode = OpCode::from_u8(self.instructions[instruction_pointer]).unwrap();

            let argument = match op_code.arity() {
                0 => "".to_string(),
                1 => format!("{}", self.instructions[instruction_pointer + 1]),
                4 => {
                    let address_bytes: [u8; 4] = self.instructions
                        [instruction_pointer + 1..instruction_pointer + 5]
                        .try_into()
                        .unwrap();
                    let address: u32 = u32::from_be_bytes(address_bytes);
                    format!("{address}")
                }
                _ => unreachable!(),
            };

            writeln!(f, "{instruction_counter:02} | {op_code:?} {argument}").unwrap();
            instruction_pointer += 1 + op_code.arity();
            instruction_counter += 1;
        }

        writeln!(f, "{}", separator)?;
        writeln!(f, "CONSTANTS:")?;
        writeln!(f, "{}", separator)?;
        for (index, constant) in self.constants.iter().enumerate() {
            writeln!(f, "{index:02} | {constant:?}")?;
        }
        writeln!(f, "{}", separator)
    }
}

#[derive(Debug)]
#[repr(u8)]
pub enum OpCode {
    Return = 0,

    /// Reads the next byte as the index of the constant
    GetConstant,
    Print,

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

    // Local Manipulation OpCodes
    GetLocal,
    SetLocal,
    Pop,

    // Jumps
    Jump,
    JumpIfTrue,
    JumpIfFalse,
}

impl OpCode {
    /// Returns the number of bytes following this OpCode that are it's arguments
    pub fn arity(&self) -> usize {
        match self {
            OpCode::GetConstant => 1,
            OpCode::GetLocal => 1,
            OpCode::SetLocal => 1,
            OpCode::Jump => 4,
            OpCode::JumpIfTrue => 4,
            OpCode::JumpIfFalse => 4,
            _ => 0,
        }
    }

    pub fn from_u8(byte: u8) -> Option<OpCode> {
        use OpCode::*;
        let op_code: OpCode = match byte {
            b if b == Return as u8 => Return,
            b if b == GetConstant as u8 => GetConstant,
            b if b == Print as u8 => Print,

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

            // Local Manipulation OpCodes
            b if b == GetLocal as u8 => GetLocal,
            b if b == SetLocal as u8 => SetLocal,
            b if b == Pop as u8 => Pop,

            // Jumps
            b if b == Jump as u8 => Jump,
            b if b == JumpIfTrue as u8 => JumpIfTrue,
            b if b == JumpIfFalse as u8 => JumpIfFalse,
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
            Value::Int(_) => "Int".to_string(),
            Value::Bool(_) => "Bool".to_string(),
            Value::String(_) => "String".to_string(),
            // Value::Fn { .. } => "<{{Function}}>".to_string(),
            Value::Unit => "Unit".to_string(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{i}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Unit => write!(f, "()"),
        }
    }
}
