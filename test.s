one:
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
two:
push rbp
mov rbp, rsp
sub rsp, 0
push rbx
mov rbx, qword ptr [rbp + 16 * 1]
push rax
mov rax, rbx
mul qword ptr [rbp + 24 * 1]
mov rbx, rax
pop rax
mov rax, rbx
pop rbx
mov qword ptr [rbp + 32 * 1], rax
mov rsp, rbp
pop rbp
ret
main:
push rbp
mov rbp, rsp
sub rsp, 64
mov qword ptr [rbp + -8 * 1], 5
lea rax, qword ptr [rbp + -8 * 1]
mov qword ptr [rbp + -16 * 1], rax
lea rax, qword ptr [rbp + -16 * 1]
mov qword ptr [rbp + -24 * 1], rax
push rbx
mov rbx, rsp
sub rsp, 8
mov rax, qword ptr [rbp + -24 * 1]
mov rax, qword ptr [rax + 0 * 1]
push qword ptr [rax + 0 * 1]
call one
mov rax, qword ptr [rbx + -8 * 1]
mov rsp, rbx
pop rbx
mov qword ptr [rbp + -32 * 1], rax
push rbx
mov rbx, qword ptr [rbp + -32 * 1]
push rbx
mov rbx, rsp
sub rsp, 8
push rbx
mov rbx, qword ptr [rbp + -32 * 1]
add rbx, 1
mov rax, rbx
pop rbx
push rax
push qword ptr [rbp + -32 * 1]
call two
mov rax, qword ptr [rbx + -8 * 1]
mov rsp, rbx
pop rbx
add rbx, rax
mov rax, rbx
pop rbx
mov qword ptr [rbp + -40 * 1], rax
lea rax, qword ptr [rbp + -40 * 1]
mov qword ptr [rbp + -48 * 1], rax
mov qword ptr [rbp + -56 * 1], 3
mov qword ptr [rbp + -64 * 1], 0
push rbx
mov rbx, rsp
push rbx
mov rbx, qword ptr [rbp + -56 * 1]
push rax
mov rax, rbx
div qword ptr [rbp + -64 * 1]
mov rbx, rax
pop rax
mov rax, rbx
pop rbx
push rax
call print_int64
mov rsp, rbx
pop rbx
mov rsp, rbp
pop rbp
ret