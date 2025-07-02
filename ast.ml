(* ast.ml *)

type expr =
  | IntLiteral of int
  | Var of string

and t =
  | DeclareQueue of string                    (* ConcurrentQueue<int> q; *)
  | Enqueue of string * expr                  (* q.enqueue(42); *)
  | TryDequeue of string * string             (* q.try_dequeue(x); *)
  | AssignBool of string * t                  (* bool ok = q.try_dequeue(x); *)
  | Assign of string * expr                   (* x = 42; or x = y; *)
  | Return of expr                            (* return x; *)

