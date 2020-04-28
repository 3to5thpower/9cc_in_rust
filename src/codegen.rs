use super::parse;
fn gen(ast: &parse::Ast) {
    use parse::AstKind::*;
    use parse::BinOpKind::*;
    use parse::UniOpKind::*;
    match ast.value.clone() {
        Num(n) => println!("  push {}", n),
        BinOp { op, l, r } => {
            gen(&l);
            gen(&r);
            println!("  pop rdi");
            println!("  pop rax");
            match op.value {
                Add => println!("  add rax, rdi"),
                Sub => println!("  sub rax, rdi"),
                Mult => println!("  imul rax, rdi"),
                Div => {
                    println!("  cqo");
                    println!("  idiv rdi");
                }
            }
            println!("  push rax");
        }
        UniOp { op, e } => match op.value {
            Plus => gen(&e),
            Minus => {
                println!("  push 0");
                gen(&e);
                println!("  pop rdi");
                println!("  pop rax");
                println!("  sub rax, rdi");
                println!("  push rax");
            }
        },
    }
}

pub fn codegen(ast: &parse::Ast) {
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    gen(&ast);
    println!("  pop rax");
    println!("  ret");
}
