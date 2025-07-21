(* directive.ml *)

(** Register types used in assembly generation *)
type register = Rax | R8 | R9 | R10 | R11 | Rsp | Rdi | Rsi | Rdx | Rbp | Rip

let string_of_register ?(byte = false) = function
  | Rax -> if byte then "al" else "rax"
  | R8 -> if byte then "r8b" else "r8"
  | R9 -> if byte then "r9b" else "r9"
  | R10 -> if byte then "r10b" else "r10"
  | R11 -> if byte then "r11b" else "r11"
  | Rsp -> "rsp"
  | Rdi -> "rdi"
  | Rsi -> "rsi"
  | Rdx -> "rdx"
  | Rbp -> "rbp"
  | Rip -> "rip"


(** Operand types for assembly *)
type operand =
  | Reg of register
  | Imm of int
  | MemOffset of (operand * operand)

let is_register = function Reg _ -> true | _ -> false

let rec string_of_operand ?(byte = false) = function
  | Reg r -> string_of_register ~byte r
  | Imm i -> Printf.sprintf "$%d" i
  | MemOffset (o1, o2) ->
      let width = if byte then "BYTE" else "QWORD" in
      Printf.sprintf "%s [%s + %s]" width
        (string_of_operand ~byte:false o1)
        (string_of_operand ~byte:false o2)

(** Assembly directives and instructions *)
type directive =
  | Global of string
  | Extern of string
  | Section of string
  | Label of string
  | DqLabel of string
  | DqString of string
  | DqInt of int
  | Align of int
  | LeaLabel of (operand * string)
  | Mov of (operand * operand)
  | MovByte of (operand * operand)
  | Add of (operand * operand)
  | Sub of (operand * operand)
  | Div of operand
  | Mul of operand
  | Cqo
  | Shl of (operand * operand)
  | Shr of (operand * operand)
  | Sar of (operand * operand)
  | Cmp of (operand * operand)
  | And of (operand * operand)
  | Or of (operand * operand)
  | Setz of operand
  | Setl of operand
  | Jmp of string
  | Je of string
  | Jne of string
  | Jl of string
  | Jnl of string
  | Jg of string
  | Jng of string
  | ComputedJmp of operand
  | Ret
  | Push of operand
  | Pop of operand
  | Call of string
  | Comment of string

let label_name macos label =
  if macos then "_" ^ label else label

let string_of_directive ~macos = function
  | Global l ->
      if macos then "default rel\nglobal _" ^ l else "global " ^ l
  | Extern l -> "extern " ^ label_name macos l
  | Section l -> "\tsection ." ^ l
  | Label l -> label_name macos l ^ ":"
  | DqLabel l -> "\tdq " ^ label_name macos l
  | DqString l -> "\tdq `" ^ String.escaped l ^ "`, 0"
  | DqInt i -> Printf.sprintf "\tdq %d" i
  | Align i -> Printf.sprintf "align %d" i
  | Mov (d, s) -> Printf.sprintf "\tmov %s, %s" (string_of_operand d) (string_of_operand s)
  | MovByte (d, s) -> Printf.sprintf "\tmov %s, %s" (string_of_operand ~byte:true d) (string_of_operand ~byte:true s)
  | Add (d, s) -> Printf.sprintf "\tadd %s, %s" (string_of_operand d) (string_of_operand s)
  | Sub (d, s) -> Printf.sprintf "\tsub %s, %s" (string_of_operand d) (string_of_operand s)
  | Div s -> Printf.sprintf "\tidiv QWORD %s" (string_of_operand s)
  | Mul s -> Printf.sprintf "\timul QWORD %s" (string_of_operand s)
  | Cqo -> "\tcqo"
  | Shl (d, s) -> Printf.sprintf "\tshl %s, %s" (string_of_operand d) (string_of_operand s)
  | Shr (d, s) -> Printf.sprintf "\tshr %s, %s" (string_of_operand d) (string_of_operand s)
  | Sar (d, s) -> Printf.sprintf "\tsar %s, %s" (string_of_operand d) (string_of_operand s)
  | Cmp (d, s) -> Printf.sprintf "\tcmp %s, %s" (string_of_operand d) (string_of_operand s)
  | And (d, s) -> Printf.sprintf "\tand %s, %s" (string_of_operand d) (string_of_operand s)
  | Or (d, s) -> Printf.sprintf "\tor %s, %s" (string_of_operand d) (string_of_operand s)
  | Setz d -> Printf.sprintf "\tsete %s" (string_of_operand ~byte:true d)
  | Setl d -> Printf.sprintf "\tsetl %s" (string_of_operand ~byte:true d)
  | LeaLabel (d, l) -> Printf.sprintf "\tlea %s, [%s]" (string_of_operand d) (label_name macos l)
  | Jmp l -> Printf.sprintf "\tjmp %s" (label_name macos l)
  | Je l -> Printf.sprintf "\tje %s" (label_name macos l)
  | Jne l -> Printf.sprintf "\tjne %s" (label_name macos l)
  | Jl l -> Printf.sprintf "\tjl %s" (label_name macos l)
  | Jnl l -> Printf.sprintf "\tjnl %s" (label_name macos l)
  | Jg l -> Printf.sprintf "\tjg %s" (label_name macos l)
  | Jng l -> Printf.sprintf "\tjng %s" (label_name macos l)
  | ComputedJmp o -> Printf.sprintf "\tjmp %s" (string_of_operand o)
  | Push o -> Printf.sprintf "\tpush %s" (string_of_operand o)
  | Pop o -> Printf.sprintf "\tpop %s" (string_of_operand o)
  | Call l -> Printf.sprintf "\tcall %s" (label_name macos l)
  | Ret -> "\tret"
  | Comment s -> "; " ^ s
