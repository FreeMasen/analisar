# Analisar

A Lua parser for Rust

## Usage


### Parser

This crate provides 3 different APIs for parsing lua. The first is a fairly standard parser over a
fairly standard AST which provides no context at all about whitespace or the position of punctuation or keywords.
The provided AST is designed to represent the intent of the program over anything else

This kind of parser could be used to build a tree walking interpreter. Here is an example of the output:

```rust
use analisar::Parser;

fn main() {
    let lua = "print('hello world')";
    let mut p = Parser::new(lua.as_bytes());
    println!("{:#?}", p.next());
}
```

```sh
cargo run
Some(
    Ok(
        Expression(
            FuncCall(
                FunctionCall {
                    prefix: Name(
                        Name {
                            name: "print",
                            attr: None,
                        },
                    ),
                    args: ExpList(
                        [
                            LiteralString(
                                LiteralString(
                                    "\'hello world\'",
                                ),
                            ),
                        ],
                    ),
                    method: false,
                },
            ),
        ),
    ),
)
```

### TokenBufferParser

This is one is a bit of a hybrid of the other two parsers provided by this crate. It provides
both a tree of Statement/Expressions but also the raw tokens represented by a given Statement.

Here is an example showing the output of this kind of parser:

```rust
use analisar::TokenBufferParser;

fn main() {
    let lua = "
    -- print out hello world to stdout
    print(--[[ why!!! ]]'hello world' --[[seriously???]])";
    let mut p = TokenBufferParser::new(lua.as_bytes());
    println!("{:#?}", p.next());
}
```

```
cargo run
Some(
    Ok(
        (
            [
                Item {
                    token: Comment(
                        "-- print out hello world to stdout",
                    ),
                    span: Span {
                        start: 5,
                        end: 39,
                    },
                },
                Item {
                    token: Name(
                        "print",
                    ),
                    span: Span {
                        start: 44,
                        end: 49,
                    },
                },
                Item {
                    token: Punct(
                        OpenParen,
                    ),
                    span: Span {
                        start: 49,
                        end: 50,
                    },
                },
                Item {
                    token: Comment(
                        "--[[ why!!! ]]",
                    ),
                    span: Span {
                        start: 50,
                        end: 64,
                    },
                },
                Item {
                    token: LiteralString(
                        "\'hello world\'",
                    ),
                    span: Span {
                        start: 64,
                        end: 77,
                    },
                },
                Item {
                    token: Comment(
                        "--[[seriously???]]",
                    ),
                    span: Span {
                        start: 78,
                        end: 96,
                    },
                },
                Item {
                    token: Punct(
                        CloseParen,
                    ),
                    span: Span {
                        start: 96,
                        end: 97,
                    },
                },
            ],
            Expression(
                FuncCall(
                    FunctionCall {
                        prefix: Name(
                            Name {
                                name: "print",
                                attr: None,
                            },
                        ),
                        args: ExpList(
                            [
                                LiteralString(
                                    LiteralString(
                                        "\'hello world\'",
                                    ),
                                ),
                            ],
                        ),
                        method: false,
                    },
                ),
            ),
        ),
    ),
)
```

As you can see the output of the `Statement::Expression` is exactly the same as before,
however there is also a `Vec` of tokens provided. 

### aware::Parser

The final parser that this crate provides is a fully context aware parser. Let's look
at an example of that the output of this one looks like.

```rust
use analisar::aware::Parser;

fn main() {
    let lua = "print(('hello world')) -- print the string 'hello world' to stdout";
    let mut p = Parser::new(lua.as_bytes());
    println!("{:#?}", p.next());
}

```

```sh
cargo run
Some(
    Ok(
        StatementWithComments {
            statement: Expression(
                FuncCall(
                    FunctionCall {
                        prefix: Name(
                            Name {
                                name_span: Span {
                                    start: 0,
                                    end: 5,
                                },
                                name: "print",
                                attr: None,
                            },
                        ),
                        args: ExpList {
                            open_paren: Span {
                                start: 5,
                                end: 6,
                            },
                            exprs: [
                                Expr(
                                    Parened {
                                        open_span: Span {
                                            start: 6,
                                            end: 7,
                                        },
                                        expr: LiteralString(
                                            LiteralString {
                                                span: Span {
                                                    start: 7,
                                                    end: 20,
                                                },
                                                value: "\'hello world\'",
                                            },
                                        ),
                                        close_span: Span {
                                            start: 20,
                                            end: 21,
                                        },
                                    },
                                ),
                            ],
                            close_paren: Span {
                                start: 21,
                                end: 22,
                            },
                        },
                    },
                ),
            ),
            comments: [
                Item {
                    token: Comment(
                        "-- print the string \'hello world\' to stdout",
                    ),
                    span: Span {
                        start: 23,
                        end: 66,
                    },
                },
            ],
        },
    ),
)
```

Notice this one looks quite a bit different from the other two. First of all the function call's name
has an associated `Span` which represents to byte offsets for the token in the original string, you'll
notice similar spans across each entry in this tree. Another thing it provides is a `Parened` expression,
for representing when an expression has been put in parentheses. Finally we see the comments that apply to
this statement are also provided. With all 3 of these additions it would be possible to fully
reconstruct the tokens into the order they appeared originally, which would be handy it you were building
a code formatter or a document generator.
