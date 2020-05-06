use super::{parse, parse::Ast};

static mut IF: usize = 0;

fn gen_val(ast: &Ast) {
    match ast.value.clone() {
        parse::AstKind::Variable(offset) => {
            println!("  mov rax, rbp");
            println!("  sub rax, {}", offset);
            println!("  push rax");
        }
        _ => unreachable!(),
    }
}

fn gen(ast: &Ast) {
    use parse::AstKind::*;
    use parse::BinOpKind::*;
    use parse::UniOpKind::*;
    match ast.value.clone() {
        If { cond, expr, els } => {
            gen(&cond);
            println!("  pop rax");
            println!("  cmp rax, 0");
            unsafe {
                match els {
                    None => {
                        println!("  je .Lend{}", IF);
                        gen(&expr);
                        println!(".Lend{}:", IF);
                    }
                    Some(ast) => {
                        println!("  je .Lelse{}", IF);
                        gen(&expr);
                        println!("  jmp .Lend{}", IF);
                        println!(".Lelse{}:", IF);
                        gen(&ast);
                        println!(".Lend{}:", IF);
                    }
                }
                IF += 1;
            }
        }
        Return(exp) => {
            gen(&exp);
            println!("  pop rax");
            println!("  mov rsp, rbp");
            println!("  pop rbp");
            println!("  ret");
        }
        Assign { l, r } => {
            gen_val(&l);
            gen(&r);
            println!("  pop rdi");
            println!("  pop rax");
            println!("  mov [rax], rdi");
            println!("  push rdi");
        }
        Stmt(ast) => gen(&ast),
        Variable(_) => {
            gen_val(&ast);
            println!("  pop rax");
            println!("  mov rax, [rax]");
            println!("  push rax");
        }
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
                Less => {
                    println!("  cmp rax, rdi");
                    println!("  setl al");
                    println!("  movzb rax, al");
                }
                LessEqual => {
                    println!("  cmp rax, rdi");
                    println!("  setle al");
                    println!("  movzb rax, al");
                }
                Equal => {
                    println!("  cmp rax, rdi");
                    println!("  sete al");
                    println!("  movzb rax, al");
                }
                NotEqual => {
                    println!("  cmp rax, rdi");
                    println!("  setne al");
                    println!("  movzb rax, al");
                }
                Greater => {
                    println!("  cmp rdi, rax");
                    println!("  setl al");
                    println!("  movzb rax, al");
                }
                GreaterEqual => {
                    println!("  cmp rdi, rax");
                    println!("  setle al");
                    println!("  movzb rax, al");
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

pub fn codegen(astes: &Vec<Ast>) {
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    //変数26個分の領域を確保
    println!("  push rbp");
    println!("  mov rbp, rsp");
    println!("  sub rsp, 208");

    for ast in astes {
        gen(ast);
        println!("  pop rax");
    }

    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}
