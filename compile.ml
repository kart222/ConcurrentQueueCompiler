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


let emit_expr (expr : expr) : directive list * operand =
  match expr with
  | IntLiteral n -> 
      [], Imm n
  | Var _ -> 
      [], Reg Rax  

let emit_stmt (stmt : t) : directive list =
  match stmt with
  (* DeclareQueue: allocate and initialize queue on stack *)
  | DeclareQueue (qname, slots) ->
      [
        Comment ("DeclareQueue " ^ qname);

        (* Load slots into Rax *)
        Mov (Reg Rax, Imm slots);

        (* Save stack pointer as buffer base in R8 *)
        Mov (Reg R8, Reg Rsp);

        (* Calculate buffer size = slots * 8 bytes *)
        Shl (Reg Rax, Imm 3);

        (* Calculate buffer end address = base + size, store in R9 *)
        Mov (Reg R9, Reg R8);
        Add (Reg R9, Reg Rax);

        (* Initialize head (R10) and tail (R11) indices to 0 *)
        Mov (Reg R10, Imm 0);
        Mov (Reg R11, Imm 0);

        (* Allocate space on stack *)
        Sub (Reg Rsp, Reg Rax);
      ]

  (* Enqueue: insert element at tail *)
| Enqueue (qname, expr) ->
    let expr_code, expr_op = emit_expr expr in
    let end_label = gensym (qname ^ "_enqueue_end") in
    let full_label = gensym (qname ^ "_enqueue_full") in
    expr_code @ [

      (* Calculate slots = (buffer_end - buffer_base) / 8 *)
      Mov (Reg Rdx, Reg R9);
      Sub (Reg Rdx, Reg R8);
      Sar (Reg Rdx, Imm 3);  (* Rdx = slots *)

      (* Calculate next_tail = (tail + 1) % slots *)
      Mov (Reg Rax, Reg R11);
      Add (Reg Rax, Imm 1);
      Cmp (Reg Rax, Reg Rdx);
      Jl (full_label ^ "_continue");
      Mov (Reg Rax, Imm 0);
      Label (full_label ^ "_continue");

      (* Check if next_tail == head *)
      Cmp (Reg Rax, Reg R10);
      Je full_label;

      (* Proceed with enqueue *)

      (* Calculate address: buffer_base (R8) + tail (R11) * 8 *)
      Mov (Reg Rax, Reg R11);
      Shl (Reg Rax, Imm 3);
      Add (Reg Rax, Reg R8);

      (* Store value at buffer[tail] *)
      Mov (MemOffset (Reg Rax, Imm 0), expr_op);

      (* Increment tail *)
      Add (Reg R11, Imm 1);

      (* Wrap tail to zero if needed *)
      Cmp (Reg R11, Reg Rdx);
      Jne end_label;
      Mov (Reg R11, Imm 0);

      Label end_label;

      (* Handle full queue *)
      Label full_label;
      Jmp "error";
    ]


  (* TryDequeue: attempt to dequeue, load value into Rsi, jump if empty *)
  | TryDequeue (qname) ->
      let empty_label = gensym (qname ^ "_trydeq_empty") in
      let done_label = gensym (qname ^ "_trydeq_done") in
      [
        Comment ("TryDequeue from " ^ qname);

        (* Compare head and tail *)
        Cmp (Reg R10, Reg R11);
        Je empty_label;

        (* Calculate address: buffer_base (R8) + head (R10) * 8 *)
        Mov (Reg Rax, Reg R10);
        Shl (Reg Rax, Imm 3);
        Add (Reg Rax, Reg R8);

        (* Load value into Rsi *)
        Mov (Reg Rsi, MemOffset (Reg Rax, Imm 0));

        (* Increment head *)
        Add (Reg R10, Imm 1);

        (* Wrap head if needed *)
        Mov (Reg Rdx, Reg R9);
        Sub (Reg Rdx, Reg R8);
        Sar (Reg Rdx, Imm 3);

        Cmp (Reg R10, Reg Rdx);
        Jne done_label;
        Mov (Reg R10, Imm 0);

        Label done_label;

        Label empty_label;
        Jmp "error";
      ]

  (* Assign: assign value to variable (placeholder using Rax) *)
  | Assign (x, expr) ->
      let expr_code, expr_op = emit_expr expr in
      expr_code @ [
        Comment ("Assign " ^ x);
        Mov (Reg Rax, expr_op);
      ]

  (* AssignBool: assign bool variable based on TryDequeue success *)
  | AssignBool (x, TryDequeue (qname)) ->
      let empty_label = gensym (qname ^ "_trydeq_empty_assignbool") in
      let store_label = gensym (qname ^ "_trydeq_store_assignbool") in
      let done_label = gensym (qname ^ "_trydeq_done_assignbool") in
      [
        Comment ("AssignBool " ^ x ^ " = TryDequeue from " ^ qname);

        (* Check if empty *)
        Cmp (Reg R10, Reg R11);
        Je empty_label;

        (* Calculate address: buffer_base + head * 8 *)
        Mov (Reg Rax, Reg R10);
        Shl (Reg Rax, Imm 3);
        Add (Reg Rax, Reg R8);

        (* Load value *)
        Mov (Reg Rsi, MemOffset (Reg Rax, Imm 0));

        (* Increment head *)
        Add (Reg R10, Imm 1);

        (* Wrap head *)
        Mov (Reg Rdx, Reg R9);
        Sub (Reg Rdx, Reg R8);
        Sar (Reg Rdx, Imm 3);

        Cmp (Reg R10, Reg Rdx);
        Jne store_label;
        Mov (Reg R10, Imm 0);

        Label store_label;

        (* Store true in Rax *)
        Mov (Reg Rax, Imm 1);

        Jmp done_label;

        Label empty_label;
        Jmp "error";
      ]

  | AssignBool _ ->
      failwith ("AssignBool only accepts TryDequeue")

  (* Return statement *)
  | Return expr ->
      let expr_code, expr_op = emit_expr expr in
      expr_code @ [
        Comment "Return statement";
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
