#![allow(dead_code)]
use anyhow::{anyhow, bail, Result};
use std::env;

mod ast;
mod codegen;
mod error;
mod lex;
mod parse;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        bail!("引数の個数が正しくありません");
    }

    let astv = parse::parse(&args[1]).map_err(|e| {
        if let Some(err) = e.downcast_ref::<error::ParseError>() {
            err.show_diagnostic(&args[1]);
            eprintln!("{}", e.root_cause());
        } else if let Some(err) = e.downcast_ref::<error::LexError>() {
            err.show_diagnostic(&args[1]);
            eprintln!("{}", e.root_cause());
        }
        anyhow!(e)
    })?;

    /*for ast in astv.iter() {
        println!("{:?}", ast);
    }*/
    let res = codegen::codegen(&astv);
    println!("{}", res);
    Ok(())
}
