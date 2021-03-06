use std::path::PathBuf;

use analisar::{aware, Error, Parser, TokenBufferParser};
use log::info;

#[test]
fn default_try_all() {
    try_all(|bytes, path| {
        let mut p = Parser::new(bytes);
        while let Some(res) = p.next() {
            check_res(bytes, &path, res)
        }
    });
}

#[test]
fn token_buffer_try_all() {
    try_all(|bytes, path| {
        let mut p = TokenBufferParser::new(bytes);
        while let Some(res) = p.next() {
            check_res(bytes, &path, res)
        }
    });
}

#[test]
fn aware_try_all() {
    try_all(|bytes, path| {
        let mut p = aware::Parser::new(bytes);
        while let Some(res) = p.next() {
            check_res(bytes, &path, res)
        }
    });
}

#[track_caller]
fn try_all(cb: impl Fn(&[u8], PathBuf)) {
    pretty_env_logger::try_init().ok();
    let lua_repo_dir = std::env::var("LUA_REPO_DIR").unwrap_or_else(|_| "./lua".to_string());
    let lua_repo_path = PathBuf::from(lua_repo_dir);
    println!(
        "start test with lua testes dir: {}",
        lua_repo_path.display()
    );
    assert!(
        lua_repo_path.exists(),
        "Unable to find lua repo directory, lua repo dir: {}",
        lua_repo_path.display()
    );
    let lua_tests_path = lua_repo_path.join("testes");
    assert!(
        lua_tests_path.exists(),
        "Unable to find the lua testes directory at {}",
        lua_tests_path.display()
    );
    for ent in std::fs::read_dir(&lua_tests_path).unwrap() {
        let entry = ent.unwrap();
        if entry
            .path()
            .extension()
            .as_ref()
            .map(|e| *e == "lua")
            .unwrap_or(false)
        {
            eprintln!("trying {}", entry.path().display());
            let text = std::fs::read(entry.path()).unwrap();
            cb(&text, entry.path());
        }
    }
}

use std::fmt::Debug;
#[track_caller]
fn check_res<T: Debug>(text: &[u8], path: &PathBuf, res: Result<T, analisar::Error>) {
    match res {
        Ok(c) => {
            info!("{:?}", c)
        }
        Err(Error::UnexpectedToken(offset, msg)) => {
            if let Some(line) = analisar::error::find_line(&text, offset) {
                let path = format!("{}:{}:{}", path.display(), line.number, line.offset);
                let msg = format!(
                    "\n{}\n{}\n{}\n{}",
                    path,
                    String::from_utf8_lossy(&text[line.start..line.start + line.length]),
                    "^".repeat(line.length),
                    msg
                );
                panic!("{}", msg);
            } else {
                panic!("{}-{}", offset, msg);
            }
        }
        Err(Error::UnexpectedEof) => panic!("Unexpected EOF"),
    }
}
