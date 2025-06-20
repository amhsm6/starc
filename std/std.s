    .intel_syntax noprefix

    .section .data

number: .ascii "%d\n\0"

true: .ascii "true\n\0"
false: .ascii "false\n\0"

    .section .text
    .global print_int64
    .global print_bool

print_int64:
    mov rdi, offset number
    mov rsi, qword ptr [rsp + 8]

    mov rbx, rsp
    and rsp, -16
    call printf
    mov rsp, rbx

    ret

print_bool:
    mov rax, qword ptr [rsp + 8]

    test rax, rax
    jz print_false

print_true:
    mov rdi, offset true
    jmp print

print_false:
    mov rdi, offset false

print:
    mov rbx, rsp
    and rsp, -16
    call printf
    mov rsp, rbx

    ret
