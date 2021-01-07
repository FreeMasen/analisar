# Analisar

A Lua parser for Rust

## Example

```rust
use analisar::Parser;

fn main() {
    let lua = "print('hello world')";
    let mut p = Parser::new(lua.as_bytes());
    println!("{:#?}", p.chunk());
}
```

```sh
cargo run
Chunk(
    Block {
        statements: [
            Expression(
                Prefix(
                    FunctionCall(
                        FunctionCall {
                            prefix: Exp(
                                Name(
                                    Name {
                                        name: "print",
                                        attr: None,
                                    },
                                ),
                            ),
                            args: String(
                                LiteralString(
                                    "\'hello world\'",
                                ),
                            ),
                            method: false,
                        },
                    ),
                ),
            ),
        ],
        ret_stat: None,
    },
)
```