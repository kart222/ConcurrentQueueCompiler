global _start
extern exit
	section .text
_start:
; DeclareQueue jobs
	mov rax, $4
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
	jl jobs_enqueue_full__1_continue
	mov rax, $0
jobs_enqueue_full__1_continue:
	cmp rax, r10
	je jobs_enqueue_full__1
	mov rax, r11
	shl rax, $3
	add rax, r8
	mov QWORD [rax + $0], $99
	add r11, $1
	cmp r11, rdx
	jne jobs_enqueue_end__0
	mov r11, $0
jobs_enqueue_end__0:
jobs_enqueue_full__1:
	jmp error
; AssignBool result = TryDequeue from jobs
	cmp r10, r11
	je jobs_trydeq_empty_assignbool__2
	mov rax, r10
	shl rax, $3
	add rax, r8
	mov rsi, QWORD [rax + $0]
	add r10, $1
	mov rdx, r9
	sub rdx, r8
	sar rdx, $3
	cmp r10, rdx
	jne jobs_trydeq_store_assignbool__3
	mov r10, $0
jobs_trydeq_store_assignbool__3:
	mov rax, $1
	jmp jobs_trydeq_done_assignbool__4
jobs_trydeq_empty_assignbool__2:
	jmp error
; Assign x
	mov rax, $5
; Return statement
	mov rax, rax
	ret
error:
; error
	mov rdi, $1
	call exit
