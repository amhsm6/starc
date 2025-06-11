    .intel_syntax noprefix
    .global _start

    .section .text

# main() {
# x := 5
# y := x + 3
# print(x + y)
# }
#
# x - int - 4 [rsp] stack_offset: 0
# y - int - 4 [rsp + 4] stack_offset: 4
# end>>>
# stack_offset = 8 - bytes to reserve on stack
#
# main:
# ReserveStack(8)
# StoreStack(0, Immediate(5))
# LoadStack(Register(EAX), 0)

# rsp    : x1 x2 x3 x4
# rsp + 4: y1 y2 y3 y4
# rsp + 8: ..

main:
    lea rsp, [rsp - 4 * 2]

    mov dword ptr [rsp], 5

    mov eax, dword ptr [rsp]
    add eax, 3
    mov dword ptr [rsp + 4], eax

    mov eax, dword ptr [rsp]
    add eax, dword ptr [rsp + 4]

    mov rdi, offset msg
    mov rsi, rax
    call printf

    lea rsp, [rsp + 4 * 2]

    ret

_start:
    call main

    mov rax, 60
    mov rdi, 0
    syscall

    ret


    .section .data

msg: .ascii "Hello, world %d\n\0"
