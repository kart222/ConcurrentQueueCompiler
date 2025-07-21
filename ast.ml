(* ast.ml *)

type expr =
  | IntLiteral of int
  | Var of string

and t =
  | DeclareQueue of string * int
  | Enqueue of string * expr
  | TryDequeue of string
  | AssignBool of string * t
  | Assign of string * expr
  | Return of expr
