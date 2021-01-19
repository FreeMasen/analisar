use analisar::aware::Parser;

fn main() {
    let lua = "print(('hello world')) -- print the string 'hello world' to stdout";
    let mut p = Parser::new(lua.as_bytes());
    println!("{:#?}", p.next());
}
