use crate::ast::Expr;

pub struct Compiler {
    locals: Vec<Local>,
    scope_depth: u32,
}

pub struct Local {
    name: String,
    depth: u32,
}

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    fn compile(&mut self, expr: Expr) {}
}
