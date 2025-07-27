global _start
extern exit
	section .text
_start:
; DeclareQueue myQ
	mov rax, $5
	mov r8, rsp
	shl rax, $3
	mov r9, r8
	add r9, rax
	mov r10, $0
	mov r11, $0
	sub rsp, rax
	mov rdx, r9
	sub rdx, r8
	sar rdx, $3
	mov rax, r11
	add rax, $1
	cmp rax, rdx
	jl myQ_enqueue_full__1_continue
	mov rax, $0
myQ_enqueue_full__1_continue:
	cmp rax, r10
	je myQ_enqueue_full__1
	mov rax, r11
	shl rax, $3
	add rax, r8
	mov QWORD [rax + $0], $42
	add r11, $1
	cmp r11, rdx
	jne myQ_enqueue_end__0
	mov r11, $0
myQ_enqueue_end__0:
myQ_enqueue_full__1:
	jmp error
	mov rdx, r9
	sub rdx, r8
	sar rdx, $3
	mov rax, r11
	add rax, $1
	cmp rax, rdx
	jl myQ_enqueue_full__3_continue
	mov rax, $0
myQ_enqueue_full__3_continue:
	cmp rax, r10
	je myQ_enqueue_full__3
	mov rax, r11
	shl rax, $3
	add rax, r8
	mov QWORD [rax + $0], $13
	add r11, $1
	cmp r11, rdx
	jne myQ_enqueue_end__2
	mov r11, $0
myQ_enqueue_end__2:
myQ_enqueue_full__3:
	jmp error
error:
; error
	mov rdi, $1
	call exit
