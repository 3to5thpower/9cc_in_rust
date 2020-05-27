use crate::ast::{Ast, AstKind, BinOpKind, UniOpKind};
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering::SeqCst;

static REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
static LABEL: AtomicUsize = AtomicUsize::new(0);

fn gen(ast: &Ast) {
    use AstKind::*;
    use BinOpKind::*;
    use UniOpKind::*;
    match ast.value.clone() {
        #[allow(unused_variables)]
        FunDeclare { name, args, body } => {
            println!("{}:", name);
            println!("  push rbp");
            println!("  mov rbp, rsp");
            println!("  sub rsp, {}", cnt_var(&ast) + 8);
            for (i, ast) in args.iter().enumerate() {
                println!("  mov rax, rbp");
                println!("  sub rax, {}", 8 * (i + 1));
                println!("  mov [rax], {}", REGS[i]);
            }
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
            let label = LABEL.load(SeqCst);
            match els {
                None => {
                    println!("  je .Lend{}", label);
                    gen(&expr);
                    println!(".Lend{}:", label);
                }
                Some(ast) => {
                    println!("  je .Lelse{}", label);
                    gen(&expr);
                    println!("  jmp .Lend{}", label);
                    println!(".Lelse{}:", label);
                    gen(&ast);
                    println!(".Lend{}:", label);
                }
            }
            LABEL.fetch_add(1, SeqCst);
        }
        While { cond, stmt } => {
            let label = LABEL.load(SeqCst);
            println!(".Lbegin{}:", label);
            gen(&cond);
            println!("  pop rax");
            println!("  cmp rax, 0");
            println!("  je .Lend{}", label);
            gen(&stmt);
            println!("  jmp .Lbegin{}", label);
            println!(".Lend{}:", label);
            LABEL.fetch_add(1, SeqCst);
        }
        For {
            declare,
            cond,
            update,
            stmt,
        } => {
            if let Some(ast) = declare {
                gen(&ast);
            }
            let label = LABEL.load(SeqCst);
            println!(".Lbegin{}:", label);
            if let Some(ast) = cond {
                gen(&ast);
            }
            println!("  pop rax");
            println!("  cmp rax, 0");
            println!("  je .Lend{}", label);
            gen(&stmt);
            if let Some(ast) = update {
                gen(&ast);
            }
            println!("  jmp .Lbegin{}", label);
            println!(".Lend{}:", label);
            LABEL.fetch_add(1, SeqCst);
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
fn gen_val(ast: &Ast) {
    match ast.value {
        AstKind::Variable(offset) => {
            println!("  mov rax, rbp");
            println!("  sub rax, {}", offset);
            println!("  push rax");
        }
        _ => unreachable!(),
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

fn cnt_var(ast: &Ast) -> usize {
    use std::cmp::max;
    use AstKind::*;
    match ast.value.clone() {
        Num(_) => 0,
        BinOp { op: _, l, r } => max(cnt_var(&l), cnt_var(&r)),
        UniOp { op: _, e } => cnt_var(&e),
        Variable(offset) => offset,
        Stmt(ast) => cnt_var(&ast),
        Assign { l, r } => max(cnt_var(&l), cnt_var(&r)),
        Return(ast) => cnt_var(&ast),
        If { cond, expr, els } => max(
            cnt_var(&cond),
            max(cnt_var(&expr), els.map_or(0, |v| cnt_var(&v))),
        ),
        While { cond, stmt } => max(cnt_var(&cond), cnt_var(&stmt)),
        Block(v) => {
            let v: Vec<usize> = v.iter().map(|ast| cnt_var(&ast)).collect();
            *v.iter().max().unwrap_or(&0)
        }
        For {
            declare,
            cond,
            update,
            stmt,
        } => max(
            max(
                declare.map_or(0, |d| cnt_var(&d)),
                cond.map_or(0, |c| cnt_var(&c)),
            ),
            max(update.map_or(0, |u| cnt_var(&u)), cnt_var(&stmt)),
        ),
        Fun { name: _, args } => {
            let v: Vec<usize> = args.iter().map(|ast| cnt_var(&ast)).collect();
            *v.iter().max().unwrap_or(&0)
        }
        FunDeclare {
            name: _,
            args,
            body,
        } => {
            let v: Vec<usize> = args.iter().map(|ast| cnt_var(&ast)).collect();
            let u: Vec<usize> = body.iter().map(|ast| cnt_var(&ast)).collect();
            max(*v.iter().max().unwrap_or(&0), *u.iter().max().unwrap_or(&0))
        }
    }
}
