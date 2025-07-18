    .intel_syntax noprefix

    .section .data

bytemsg: .ascii "%hhd\n\0"
wordmsg: .ascii "%hd\n\0"
dwordmsg: .ascii "%d\n\0"
qwordmsg: .ascii "%lld\n\0"

true: .ascii "true\n\0"
false: .ascii "false\n\0"

    .section .text
    .global print_int64
    .global print_int32
    .global print_int16
    .global print_int8
    .global print_bool

print_int64:
    push rsi
    push rbx

    mov rdi, offset qwordmsg
    mov rsi, qword ptr [rsp + 24]

    mov rbx, rsp
    and rsp, -16
    call printf
    mov rsp, rbx

    pop rbx
    pop rsi

    ret

print_int32:
    push rsi
    push rbx

    mov rdi, offset dwordmsg

    mov rsi, 0
    mov esi, dword ptr [rsp + 24]

    mov rbx, rsp
    and rsp, -16
    call printf
    mov rsp, rbx

    pop rbx
    pop rsi

    ret

print_int16:
    push rsi
    push rbx

    mov rdi, offset wordmsg
    movzx rsi, word ptr [rsp + 24]

    mov rbx, rsp
    and rsp, -16
    call printf
    mov rsp, rbx

    pop rbx
    pop rsi

    ret

print_int8:
    push rsi
    push rbx

    mov rdi, offset bytemsg
    movzx rsi, byte ptr [rsp + 24]

    mov rbx, rsp
    and rsp, -16
    call printf
    mov rsp, rbx

    pop rbx
    pop rsi

    ret

print_bool:
    push rbx

    mov bl, byte ptr [rsp + 16]

    test bl, bl
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

    pop rbx

    ret
