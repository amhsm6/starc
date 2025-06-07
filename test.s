.intel_syntax noprefix

foo:
    mov rax, 1
    mov rdi, 1
    lea rsi, [rsp+8]
    mov rdx, 1
    syscall

    ret

.global main
main:
    push 0x30
    call foo
