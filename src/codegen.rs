use crate::ast::{Ast, AstKind, BinOpKind, UniOpKind};
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering::SeqCst;

static REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
static LABEL: AtomicUsize = AtomicUsize::new(0);

fn gen(out: &mut String, ast: &Ast) {
    use AstKind::*;
    use BinOpKind::*;
    use UniOpKind::*;
    match ast.value.clone() {
        FunDeclare { name, args, body } => {
            out.push_str(&format!("{}:\n", name));
            out.push_str("  push rbp\n");
            out.push_str("  mov rbp, rsp\n");
            out.push_str(&format!("  sub rsp, {}\n", cnt_var(&ast) + 8));
            for i in 0..args.len() {
                out.push_str("  mov rax, rbp\n");
                out.push_str(&format!("  sub rax, {}\n", 8 * (i + 1)));
                out.push_str(&format!("  mov [rax], {}\n", REGS[i]));
            }
            for ast in body {
                gen(out, &ast);
                out.push_str("  pop rax\n");
            }
            out.push_str("  mov rsp, rbp\n");
            out.push_str("  pop rbp\n");
            out.push_str("  ret\n");
        }
        Fun { name, args } => {
            for (i, ast) in args.iter().enumerate() {
                gen(out, &ast);
                out.push_str("  pop rax\n");
                out.push_str(&format!("  mov {}, rax\n", REGS[i]));
            }
            out.push_str(&format!("  call {}\n", name));
            out.push_str("  push rax\n");
        }
        Block(vec) => {
            for ast in vec {
                gen(out, &ast);
                out.push_str("  pop rax\n");
            }
        }
        If { cond, expr, els } => {
            gen(out, &cond);
            out.push_str("  pop rax\n");
            out.push_str("  cmp rax, 0\n");
            let label = LABEL.load(SeqCst);
            match els {
                None => {
                    out.push_str(&format!("  je .Lend{}\n", label));
                    gen(out, &expr);
                    out.push_str(&format!(".Lend{}:\n", label));
                }
                Some(ast) => {
                    out.push_str(&format!("  je .Lelse{}\n", label));
                    gen(out, &expr);
                    out.push_str(&format!("  jmp .Lend{}\n", label));
                    out.push_str(&format!(".Lelse{}:\n", label));
                    gen(out, &ast);
                    out.push_str(&format!(".Lend{}:\n", label));
                }
            }
            LABEL.fetch_add(1, SeqCst);
        }
        While { cond, stmt } => {
            let label = LABEL.load(SeqCst);
            out.push_str(&format!(".Lbegin{}:\n", label));
            gen(out, &cond);
            out.push_str("  pop rax\n");
            out.push_str("  cmp rax, 0\n");
            out.push_str(&format!("  je .Lend{}\n", label));
            gen(out, &stmt);
            out.push_str(&format!("  jmp .Lbegin{}\n", label));
            out.push_str(&format!(".Lend{}:\n", label));
            LABEL.fetch_add(1, SeqCst);
        }
        For {
            declare,
            cond,
            update,
            stmt,
        } => {
            if let Some(ast) = declare {
                gen(out, &ast);
            }
            let label = LABEL.load(SeqCst);
            out.push_str(&format!(".Lbegin{}:\n", label));
            if let Some(ast) = cond {
                gen(out, &ast);
            }
            out.push_str("  pop rax\n");
            out.push_str("  cmp rax, 0\n");
            out.push_str(&format!("  je .Lend{}\n", label));
            gen(out, &stmt);
            if let Some(ast) = update {
                gen(out, &ast);
            }
            out.push_str(&format!("  jmp .Lbegin{}\n", label));
            out.push_str(&format!(".Lend{}:\n", label));
            LABEL.fetch_add(1, SeqCst);
        }
        Return(exp) => {
            gen(out, &exp);
            out.push_str("  pop rax\n");
            out.push_str("  mov rsp, rbp\n");
            out.push_str("  pop rbp\n");
            out.push_str("  ret\n");
        }
        Assign { l, r } => {
            gen_addr(out, &l);
            gen(out, &r);
            out.push_str("  pop rdi\n");
            out.push_str("  pop rax\n");
            out.push_str("  mov [rax], rdi\n");
            out.push_str("  push rdi\n");
        }
        Stmt(ast) => gen(out, &ast),
        Variable(_) => {
            gen_addr(out, &ast);
            out.push_str("  pop rax\n");
            out.push_str("  mov rax, [rax]\n");
            out.push_str("  push rax\n");
        }
        Num(n) => out.push_str(&format!("  push {}\n", n)),
        BinOp { op, l, r } => {
            gen(out, &l);
            gen(out, &r);
            out.push_str("  pop rdi\n");
            out.push_str("  pop rax\n");
            match op.value {
                Add => out.push_str("  add rax, rdi\n"),
                Sub => out.push_str("  sub rax, rdi\n"),
                Mult => out.push_str("  imul rax, rdi\n"),
                Div => {
                    out.push_str("  cqo\n");
                    out.push_str("  idiv rdi\n");
                }
                Less => {
                    out.push_str("  cmp rax, rdi\n");
                    out.push_str("  setl al\n");
                    out.push_str("  movzb rax, al\n");
                }
                LessEqual => {
                    out.push_str("  cmp rax, rdi\n");
                    out.push_str("  setle al\n");
                    out.push_str("  movzb rax, al\n");
                }
                Equal => {
                    out.push_str("  cmp rax, rdi\n");
                    out.push_str("  sete al\n");
                    out.push_str("  movzb rax, al\n");
                }
                NotEqual => {
                    out.push_str("  cmp rax, rdi\n");
                    out.push_str("  setne al\n");
                    out.push_str("  movzb rax, al\n");
                }
                Greater => {
                    out.push_str("  cmp rdi, rax\n");
                    out.push_str("  setl al\n");
                    out.push_str("  movzb rax, al\n");
                }
                GreaterEqual => {
                    out.push_str("  cmp rdi, rax\n");
                    out.push_str("  setle al\n");
                    out.push_str("  movzb rax, al\n");
                }
            }
            out.push_str("  push rax\n");
        }
        UniOp { op, e } => match op.value {
            Plus => gen(out, &e),
            Minus => {
                out.push_str("  push 0\n");
                gen(out, &e);
                out.push_str("  pop rdi\n");
                out.push_str("  pop rax\n");
                out.push_str("  sub rax, rdi\n");
                out.push_str("  push rax\n");
            }
            Reference => {
                gen_addr(out, &e);
            }
            Dereference => {
                gen(out, &e);
                out.push_str("  pop rax\n");
                out.push_str("  mov rax, [rax]\n");
                out.push_str("  push rax\n");
            }
        },
    }
}
fn gen_addr(out: &mut String, ast: &Ast) {
    match ast.value {
        AstKind::Variable(offset) => {
            out.push_str("  mov rax, rbp\n");
            out.push_str(&format!("  sub rax, {}\n", offset));
            out.push_str("  push rax\n");
        }
        _ => unreachable!(),
    }
}

pub fn codegen(astes: &Vec<Ast>) -> String {
    let mut res = String::new();
    res.push_str(".intel_syntax noprefix\n");
    res.push_str(".global main\n");
    for ast in astes {
        gen(&mut res, ast);
        res.push_str("  pop rax\n");
    }
    res
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
