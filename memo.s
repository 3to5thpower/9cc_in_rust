.intel_syntax noprefix
.global main
main:
  push rbp
  mov rbp, rsp
  sub rsp, 24
  mov rax, rbp
  sub rax, 8
  mov rdi, 0
  mov [rax], rdi
  mov rax, rbp
  sub rax, 16
  push rax
  pop rax
  mov rdi, 0
  mov [rax], rdi
  mov rax, rbp
  sub rax, 16
  mov rdi, rbp
  sub rdi, 8
  mov [rax], rdi
  mov rax, rbp
  sub rax, 16
  mov rax, [rax]
  leave
  ret
  mov rax, [rax]
  mov rdi, 3
  mov [rax], rdi
  mov rax, rbp
  sub rax, 8
  mov rax, [rax]
  mov rsp, rbp
  pop rbp
  ret

