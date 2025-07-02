(* lexer.mll *)
{
  open Parser
  exception Error of string
}

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }
| "::" { SCOPE }
| "." { DOT }
| "<" { LT }
| ">" { GT }
| "(" { LPAREN }
| ")" { RPAREN }
| "{" { LBRACE }
| "}" { RBRACE }
| ";" { SEMICOLON }
| "," { COMMA }
| "=" { EQUAL }
| "&" { AMPERSAND }
| "*" { ASTERISK }

(* Keywords *)
| "int" { INT }
| "bool" { BOOL }
| "void" { VOID }
| "auto" { AUTO }
| "return" { RETURN }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "for" { FOR }
| "new" { NEW }

(* MoodyCamel API *)
| "ConcurrentQueue" { CONCURRENTQUEUE }
| "enqueue_bulk" { ENQUEUE_BULK }
| "try_dequeue_bulk" { TRY_DEQUEUE_BULK }
| "enqueue" { ENQUEUE }
| "try_dequeue" { TRY_DEQUEUE }

(* Identifiers and literals *)
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { IDENT id }
| ['0'-'9']+ as num { NUMBER (int_of_string num) }

| eof { EOF }
| _ as c { raise (Error (Printf.sprintf "Unexpected character: %c" c)) }

