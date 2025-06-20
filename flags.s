    .intel_syntax noprefix
    .section .text
    .global _start

_start:
    mov rax, 4
    mov rbx, 4
    cmp rax, rbx

    pushf

    mov rax, 0b0000000100000110
    bextr rbx, qword ptr [rsp], rax

    mov rcx, 1
    andn rbx, rbx, rcx

    popf

    mov rax, 60
    mov rdi, 0
    syscall

    ret
