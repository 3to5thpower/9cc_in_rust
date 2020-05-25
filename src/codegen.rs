use crate::ast::{Ast, AstKind, BinOpKind, UniOpKind};

static mut LABEL: usize = 0;
static REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

fn gen_val(ast: &Ast) {
    match ast.value.clone() {
        AstKind::Variable(offset) => {
            println!("  mov rax, rbp");
            println!("  sub rax, {}", offset);
            println!("  push rax");
        }
        _ => unreachable!(),
    }
}

fn gen(ast: &Ast) {
    use AstKind::*;
    use BinOpKind::*;
    use UniOpKind::*;
    match ast.value.clone() {
        FunDeclare {
            name,
            args: _,
            body,
        } => {
            println!("{}:", name);
            //変数10個分の領域を確保
            println!("  push rbp");
            println!("  mov rbp, rsp");
            println!("  sub rsp, 80");
            for ast in body {
                gen(&ast);
                println!("  pop rax");
            }
            println!("  mov rsp, rbp");
            println!("  pop rbp");
            println!("  ret");
        }
        Fun { name, args } => {
            for (i, ast) in args.iter().enumerate() {
                gen(&ast);
                println!("  pop rax");
                println!("  mov {}, rax", REGS[i]);
            }
            println!("  call {}", name);
            println!("  push rax");
        }
        Block(vec) => {
            for ast in vec {
                gen(&ast);
                println!("  pop rax");
            }
        }
        If { cond, expr, els } => {
            gen(&cond);
            println!("  pop rax");
            println!("  cmp rax, 0");
            unsafe {
                match els {
                    None => {
                        println!("  je .Lend{}", LABEL);
                        gen(&expr);
                        println!(".Lend{}:", LABEL);
                    }
                    Some(ast) => {
                        println!("  je .Lelse{}", LABEL);
                        gen(&expr);
                        println!("  jmp .Lend{}", LABEL);
                        println!(".Lelse{}:", LABEL);
                        gen(&ast);
                        println!(".Lend{}:", LABEL);
                    }
                }
                LABEL += 1;
            }
        }
        While { cond, stmt } => unsafe {
            println!(".Lbegin{}:", LABEL);
            gen(&cond);
            println!("  pop rax");
            println!("  cmp rax, 0");
            println!("  je .Lend{}", LABEL);
            gen(&stmt);
            println!("  jmp .Lbegin{}", LABEL);
            println!(".Lend{}:", LABEL);
            LABEL += 1;
        },
        For {
            declare,
            cond,
            update,
            stmt,
        } => unsafe {
            if let Some(ast) = declare {
                gen(&ast);
            }
            println!(".Lbegin{}:", LABEL);
            if let Some(ast) = cond {
                gen(&ast);
            }
            println!("  pop rax");
            println!("  cmp rax, 0");
            println!("  je .Lend{}", LABEL);
            gen(&stmt);
            if let Some(ast) = update {
                gen(&ast);
            }
            println!("  jmp .Lbegin{}", LABEL);
            println!(".Lend{}:", LABEL);
            LABEL += 1;
        },
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
    for ast in astes {
        gen(ast);
        println!("  pop rax");
    }
}
