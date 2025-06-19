    .intel_syntax noprefix

    .section .data

msg: .ascii "%d\n\0"

    .section .text
    .global print_int64

print_int64:
    mov rdi, offset msg
    mov rsi, qword ptr [rsp + 8]

    mov rbx, rsp
    and rsp, -16
    call printf
    mov rsp, rbx

    ret
