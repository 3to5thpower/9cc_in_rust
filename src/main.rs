use std::env;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("引数の個数が正しくありません");
        return Err("error".to_owned());
    }
    let ret: i32 = args[1].parse().unwrap();
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    println!("  mov rax, {}", ret);
    println!("  ret");
    Ok(())
}
