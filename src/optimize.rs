use regex::Regex;

pub fn optimize(input: &str) -> String {
    //function epilogue (leave ret)
    let re = Regex::new(r"ret\s+pop rax\s+mov rsp, rbp\s+pop rbp\s+ret\s+pop rax").unwrap();
    let input = re.replace_all(&input, "ret");
    let re = Regex::new(r"mov rsp, rbp\s+pop rbp\s+ret").unwrap();
    let input = re.replace_all(&input, "leave\n  ret");

    //var load: mov rax, rbp; sub rax, 8; mov rax, [rax] => mov rax, [rbp - 8]
    let re = Regex::new(r"mov rax, rbp\s+sub rax, (\d+)\s+mov ([a-z]{3}]), [rax]").unwrap();
    let input = re.replace_all(&input, "mov $2, [rbp - $1]\n");

    //push rax, pop rdi => mov rdi, rax
    let re = Regex::new(r"push (\w+)\s+pop (\w+)").unwrap();
    let input = re.replace_all(&input, "mov $2, $1");

    //push rax; pop rax => ""
    let re = Regex::new(r"push ([a-z]{3})\s+pop $1\n").unwrap();
    let input = re.replace_all(&input, "");

    //push 3; pop rdi => mov rdi, DWORD PTR 3
    let re = Regex::new(r"push (\d+)\s+pop ([a-z]{3}])\n").unwrap();
    let input = re.replace_all(&input, "mov $2, DWORD PTR $1\n");

    //mov rax, 3; push rax => push 3
    let re = Regex::new(r"mov (\w+), (\d+)\s+push $1, $2").unwrap();
    let input = re.replace_all(&input, "push $1\n");

    //push 2; push 3; pop rdi; pop rax; => mov rax, 2; mov rdi, 3;
    let re = Regex::new(r"push (\w+)\s+push (\w+)\s+pop (\w+)\s+pop (\w+)").unwrap();
    let input = re.replace_all(&input, "mov $4, $1\n  mov $3, $2");

    //mov rax, 4; mov rsi, rax => mov rsi, 4
    let re = Regex::new(r"mov rax, (\d+)\s+mov (\w+), rax").unwrap();
    let input = re.replace_all(&input, "mov $2, $1");

    //mov rax, rax => ""
    let re = Regex::new(r"\s+mov rax, rax").unwrap();
    let input = re.replace_all(&input, "");

    //var load: mov rax, rbp; sub rax, 8; mov rax, [rax] => mov rax, [rbp - 8]
    let re = Regex::new(r"mov rax, rbp\s+sub rax, (\d+)\s+mov ([a-z]{3}]), [rax]").unwrap();
    let input = re.replace_all(&input, "mov $2, [rbp - $1]\n");

    input.to_string()
}
