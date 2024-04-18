mod ast;
mod parse;

use std::env;
use std::fs;

fn line_column(bytes: &[u8], offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;
    for byte in bytes.iter().take(offset) {
        if (*byte as char) == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    (line, column)
}

fn main() {
    let path = {
        let mut args = env::args();
        args.next();
        args.next().unwrap()
    };
    let bytes = fs::read(path.clone()).unwrap();
    let mut tokenizer = parse::Tokenizer::from(&bytes[..]);
    match tokenizer.parse_stmt() {
        Ok(stmt) => println!("{}", stmt.0),
        Err(offset) => {
            let (line, column) = line_column(&bytes, offset);
            println!("{path}:{line}:{column}");
        }
    }
}
