global _start
extern exit
	section .text
_start:
; DeclareQueue q
	mov rax, $2
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
	jl q_enqueue_full__1_continue
	mov rax, $0
q_enqueue_full__1_continue:
	cmp rax, r10
	je q_enqueue_full__1
	mov rax, r11
	shl rax, $3
	add rax, r8
	mov QWORD [rax + $0], $1
	add r11, $1
	cmp r11, rdx
	jne q_enqueue_end__0
	mov r11, $0
q_enqueue_end__0:
q_enqueue_full__1:
	jmp error
	mov rdx, r9
	sub rdx, r8
	sar rdx, $3
	mov rax, r11
	add rax, $1
	cmp rax, rdx
	jl q_enqueue_full__3_continue
	mov rax, $0
q_enqueue_full__3_continue:
	cmp rax, r10
	je q_enqueue_full__3
	mov rax, r11
	shl rax, $3
	add rax, r8
	mov QWORD [rax + $0], $2
	add r11, $1
	cmp r11, rdx
	jne q_enqueue_end__2
	mov r11, $0
q_enqueue_end__2:
q_enqueue_full__3:
	jmp error
; TryDequeue from q
	cmp r10, r11
	je q_trydeq_empty__4
	mov rax, r10
	shl rax, $3
	add rax, r8
	mov rsi, QWORD [rax + $0]
	add r10, $1
	mov rdx, r9
	sub rdx, r8
	sar rdx, $3
	cmp r10, rdx
	jne q_trydeq_done__5
	mov r10, $0
q_trydeq_done__5:
q_trydeq_empty__4:
	jmp error
; Return statement
	mov rax, $0
	ret
error:
; error
	mov rdi, $1
	call exit
