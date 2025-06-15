    .intel_syntax noprefix
    .global _start

    .section .text

# main(int a, int b) {
# x := 5
# y := x + 3
# print(x + y)
# return x
# }
#
# rsp -> y1 y2 y3 y4
#        x1 x2 x3 x4
# rbp -> << previous rbp >>
#        << return address >>
#        a1 a2 a3 a4
#        b1 b2 b3 b4
#        r1 r2 r3 r4
# rsp old -> ------
#
# x [rbp-4]
# y [rbp-8]
# a [rbp+16]
# b [rbp+16+4]
# r [rbp+16+8]
#

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
