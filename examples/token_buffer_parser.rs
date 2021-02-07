use analisar::TokenBufferParser;

fn main() {
    let lua = "
    -- print out hello world to stdout
    print(--[[ why!!! ]]'hello world' --[[seriously???]])";
    let mut p = TokenBufferParser::new(lua.as_bytes());
    println!("{:#?}", p.next());
}
