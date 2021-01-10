use analisar::Parser;

fn main() {
    let lua = "print('hello world')";
    let mut p = Parser::new(lua.as_bytes());
    println!("{:#?}", p.chunk());
}
