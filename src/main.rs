#![allow(dead_code)]
use std::env;

mod codegen;
mod parse;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(format!("引数の個数が正しくありません"));
    }

    let ast = match args[1].parse::<parse::Ast>() {
        Ok(ast) => ast,
        Err(e) => {
            e.show_diagnostic(&args[1]);
            parse::show_trace(e);
            return Ok(());
        }
    };
    codegen::codegen(&ast);
    Ok(())
}
