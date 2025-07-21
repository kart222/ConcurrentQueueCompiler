open Ast
open Directive
open Util

let align_stack_index : int -> int =
  fun stack_index ->
    if stack_index mod 16 = -8 then
      stack_index
    else
      stack_index - 8

let stack_address : int -> operand =
  fun index ->
    MemOffset (Imm index, Reg Rsp)

(* Emit assembly code for expressions *)
let emit_expr (expr : expr) : directive list * operand =
  match expr with
  | IntLiteral n -> [], Imm n
  | Var x -> [], Reg Rax  (* Placeholder: variable value assumed in RAX *)


(* Emit assembly code for statements *)
let emit_stmt (stmt : t) : directive list =
  match stmt with
  | DeclareQueue (qname, slots) ->
      let end_label = gensym "end" in
      let continue_label = gensym "continue" in
      [ 
        Mov (Reg Rax, Imm slots);
        (* Instead of allocating registers for head and tail of queue, we use labels*)
        Label (qname ^ "_buffer");
        Mov (MemOffset (Reg Rsp, Imm 0), Imm 0);
        Sub (Reg Rsp, Imm (align_stack_index 0));
        Cmp (Reg Rax, Imm 0);
        Jng "error";

        Label (qname ^ "_head");
        Label continue_label;

        Mov (MemOffset (Reg Rsp, Imm 0), Imm 0);       
        Sub (Reg Rsp, Imm 8);

        Sub (Reg Rax, Imm 1);
        Cmp (Reg Rax, Imm 0);
        Je end_label;

        Jmp continue_label;

        Label (qname ^ "_tail");

        Label end_label;
        Mov (MemOffset (Reg Rsp, Imm 0), Imm 0);
        Sub (Reg Rsp, Imm (align_stack_index 0));
      ]

  | Enqueue (qname, expr) ->
      let expr_code, expr_op = emit_expr expr in
      let end_label = gensym "enqueue_end" in
      expr_code @ [
        Comment ("Enqueue into " ^ qname);

        Mov (Reg Rax, MemOffset (Reg Rip, Label (qname ^ "_tail")));
        Mov (Reg R10, MemOffset (Reg Rip, Label (qname ^ "_buffer")));
        Mov (Reg R11, Reg Rax);
        Shl (Reg R11, Imm 3);
        Add (Reg R10, Reg R11);
        Mov (MemOffset (Reg R10, Imm 0), expr_op);
        Add (Reg Rax, Imm 1);
        Cmp (Reg Rax, Imm 5);
        Jl end_label;
        Mov (Reg Rax, Imm 0);
        Label end_label;
        Mov (MemOffset (Reg Rip, Label (qname ^ "_tail")), Reg Rax);
      ]

  | TryDequeue (qname, var) ->
      let empty_label = gensym "trydeq_empty" in
      let store_label = gensym "trydeq_store" in
      let done_label = gensym "trydeq_done" in
      [
        Mov (Reg Rax, MemOffset (Reg Rip, Label (qname ^ "_head")));
        Mov (Reg R10, MemOffset (Reg Rip, Label (qname ^ "_tail")));
        Cmp (Reg Rax, Reg R10);
        Je empty_label;
        Mov (Reg R11, MemOffset (Reg Rip, Label (qname ^ "_buffer")));
        Mov (Reg Rdx, Reg Rax);
        Shl (Reg Rdx, Imm 3);
        Add (Reg R11, Reg Rdx);
        Mov (Reg Rsi, MemOffset (Reg R11, Imm 0));
        Add (Reg Rax, Imm 1);
        Cmp (Reg Rax, Imm 5);
        Jl store_label;
        Mov (Reg Rax, Imm 0);
        Label store_label;
        Mov (MemOffset (Reg Rip, Label (qname ^ "_head")), Reg Rax);
        Jmp done_label;
        Label empty_label;
        Label done_label;
      ]

  | Assign (x, expr) ->
      let expr_code, expr_op = emit_expr expr in
      expr_code @ [
        Comment ("Assign " ^ x);
        Mov (Reg Rax, expr_op);
      ]

  | AssignBool (x, TryDequeue (qname, var)) ->
      let empty_label = gensym "trydeq_empty_assignbool" in
      let store_label = gensym "trydeq_store_assignbool" in
      let done_label = gensym "trydeq_done_assignbool" in
      [
        Comment ("AssignBool " ^ x ^ " = TryDequeue from " ^ qname);
        Mov (Reg Rax, MemOffset (Reg Rip, Label (qname ^ "_head")));
        Mov (Reg R10, MemOffset (Reg Rip, Label (qname ^ "_tail")));
        Cmp (Reg Rax, Reg R10);
        Je empty_label;
        Mov (Reg R11, MemOffset (Reg Rip, Label (qname ^ "_buffer")));
        Mov (Reg Rdx, Reg Rax);
        Shl (Reg Rdx, Imm 3);
        Add (Reg R11, Reg Rdx);
        Mov (Reg Rsi, MemOffset (Reg R11, Imm 0));
        Add (Reg Rax, Imm 1);
        Cmp (Reg Rax, Imm 5);
        Jl store_label;
        Mov (Reg Rax, Imm 0);
        Label store_label;
        Mov (MemOffset (Reg Rip, Label (qname ^ "_head")), Reg Rax);
        Jmp done_label;
        Label empty_label;
        Label done_label;
      ]

  | Return expr ->
      let expr_code, expr_op = emit_expr expr in
      expr_code @ [
        Mov (Reg Rax, expr_op);
        Ret
      ]

(* Entry point to generate full assembly *)
let generate_asm (prog : t list) : directive list =
  let header = [
    Global "_start";
    Extern "exit";
    Section "text";
    Label "_start"
  ] in
  let body = List.flatten (List.map emit_stmt prog) in
  let error_handler = [
    Label "error";
    Comment "error";
    Mov (Reg Rdi, Imm 1);
    Call "exit"
  ] in
  header @ body @ error_handler
