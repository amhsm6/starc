    .intel_syntax noprefix

    .section .data

msg: .ascii "%d\n\0"

    .section .text
    .global _start

print_int64:
    mov rdi, offset msg
    mov rsi, qword ptr [rsp + 8]
    call printf

    ret

_start:
    call main

    mov rax, 60
    mov rdi, 0
    syscall

    ret
print:
push rbp
mov rbp, rsp
sub rsp, 0
push rbx
mov rbx, qword ptr [rbp + 16 * 1]
add rbx, 1
mov rax, rbx
pop rbx
mov qword ptr [rbp + 24 * 1], rax
mov rsp, rbp
pop rbp
ret
main:
push rbp
mov rbp, rsp
sub rsp, 24
mov qword ptr [rbp + -8 * 1], 5
lea rax, qword ptr [rbp + -8 * 1]
mov qword ptr [rbp + -16 * 1], rax
lea rax, qword ptr [rbp + -16 * 1]
mov qword ptr [rbp + -24 * 1], rax
push rbx
mov rbx, rsp
push rbx
mov rbx, rsp
lea rsp, qword ptr [rsp + -8 * 1]
mov rax, qword ptr [rbp + -24 * 1]
mov rax, qword ptr [rax + 0 * 1]
push qword ptr [rax + 0 * 1]
call print
mov rsp, rbx
mov rax, qword ptr [rsp + -8 * 1]
pop rbx
push rax
call print_int64
mov rsp, rbx
mov rax, 0
pop rbx
mov rsp, rbp
pop rbp
ret