one:
push rbp
mov rbp, rsp
sub rsp, 0
push rbx
mov rbx, qword ptr [rbp + 16 * 1]
add rbx, 1
mov rax, rbx
pop rbx
mov qword ptr [rsi + 0 * 1], rax
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
mov qword ptr [rsi + 0 * 1], rax
mov rsp, rbp
pop rbp
ret
main:
push rbp
mov rbp, rsp
sub rsp, 72
mov qword ptr [rbp + -8 * 1], 5
lea rax, qword ptr [rbp + -8 * 1]
mov qword ptr [rbp + -16 * 1], rax
lea rax, qword ptr [rbp + -16 * 1]
mov qword ptr [rbp + -24 * 1], rax
push rbx
push rbx
push rbx
push rsi
mov rbx, rsp
lea rsi, qword ptr [rbp + -32 * 1]
mov rax, qword ptr [rbp + -24 * 1]
mov rax, qword ptr [rax + 0 * 1]
push qword ptr [rax + 0 * 1]
call one
mov rsp, rbx
pop rsi
pop rbx
mov rbx, qword ptr [rbp + -32 * 1]
push rbx
push rsi
mov rbx, rsp
lea rsi, qword ptr [rbp + -40 * 1]
mov rax, qword ptr [rbp + -24 * 1]
mov rax, qword ptr [rax + 0 * 1]
push qword ptr [rax + 0 * 1]
call one
mov rsp, rbx
pop rsi
pop rbx
push rax
mov rax, rbx
mul qword ptr [rbp + -40 * 1]
mov rbx, rax
pop rax
mov rax, rbx
pop rbx
mov rbx, rax
push rbx
push rsi
mov rbx, rsp
lea rsi, qword ptr [rbp + -48 * 1]
mov rax, qword ptr [rbp + -24 * 1]
mov rax, qword ptr [rax + 0 * 1]
push qword ptr [rax + 0 * 1]
call one
mov rsp, rbx
pop rsi
pop rbx
push rax
mov rax, rbx
mul qword ptr [rbp + -48 * 1]
mov rbx, rax
pop rax
mov rax, rbx
pop rbx
mov qword ptr [rbp + -56 * 1], rax
mov qword ptr [rbp + -64 * 1], 3
mov qword ptr [rbp + -72 * 1], 0
push rbx
push rsi
mov rbx, rsp
lea rsi,  [rbp + -72 * 1]
push qword ptr [rbp + -56 * 1]
call print_int64
mov rsp, rbx
pop rsi
pop rbx
mov rsp, rbp
pop rbp
ret