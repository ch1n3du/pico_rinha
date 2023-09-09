# pico-rinha

Interpreters for the [Rinha]() programming language.

## structure of the repo

``` sh
.
└─── src
   └── main.rs
   └── ast.rs
   └── walker.rs
```

- [ast.rs](src/walker.rs) contains the code for serializing and deserializing the AST.
- [walker.rs](src/walker.rs) contains the code for a tree-walking interpreter.