    .intel_syntax noprefix
    .section .text
    .global _start

_start:
    jmp 1b

1:
    mov eax, 100
    jmp 1f

1:
    mov eax, -40

out:
    mov ebx, 5
    cdq
    idiv ebx

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
