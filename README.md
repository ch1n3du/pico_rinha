# pico-rinha

two smol interpreters for the [Rinha](https://github.com/aripiprazole/rinha-de-compiler/) programming language.

## structure of the repo

``` sh
.
└─── src
    ├── main.rs
    ├── cli.rs
    ├── ast.rs
    ├── walker.rs
    └── vm
       ├── mod.rs
       ├── chunk.rs
       └── compiler.rs
```

- [main.rs](src/main.rs) calls the cli.
- [cli.rs](src/cli.rs) contains the code for the CLI.
- [ast.rs](src/walker.rs) contains the code for serializing and deserializing the AST.
- [walker.rs](src/walker.rs) contains the code for a tree-walking interpreter.
- [vm](src/vm/) contains the code for the ByteCode Interpreter.
   - [vm/mod.rs](src/vm/mod.rs) contains all the code for the bytecode vm.
   - [vm/chunk.rs](src/vm/chunk.rs) contains all the code for the vm's bytecode format including the runtime representation of runtime values and the instruction set.
   - [vm/compiler.rs](src/vm/chunk.rs) contains all the code for compiling the [ast.rs](src/ast.rs)'s `Program` type to a chunk of bytecode.

> **Note:** The VM currently can't run recursive functions 😔.