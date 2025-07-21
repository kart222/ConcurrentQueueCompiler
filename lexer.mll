{
  open Parser
  exception Error of string
}

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }
| "." { DOT }
| "(" { LPAREN }
| ")" { RPAREN }
| ";" { SEMICOLON }
| "=" { EQUAL }

| "bool" { BOOL }
| "return" { RETURN }

| "ConcurrentQueue" { CONCURRENTQUEUE }
| "enqueue" { ENQUEUE }
| "try_dequeue" { TRY_DEQUEUE }

| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { IDENT id }
| ['0'-'9']+ as num { NUMBER (int_of_string num) }

| eof { EOF }
| _ as c { raise (Error (Printf.sprintf "Unexpected character: %c" c)) }
