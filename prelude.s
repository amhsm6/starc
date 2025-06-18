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
