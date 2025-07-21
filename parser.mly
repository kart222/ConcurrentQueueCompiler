%{
open Ast
%}

%token <string> IDENT
%token <int> NUMBER
%token CONCURRENTQUEUE BOOL RETURN
%token ENQUEUE TRY_DEQUEUE
%token DOT LPAREN RPAREN SEMICOLON EQUAL
%token EOF

%start <Ast.t list> program


%%

program:
| stmts EOF { $1 }

stmts:
| stmt SEMICOLON stmts { $1 :: $3 }
| stmt SEMICOLON        { [$1] }

stmt:
| CONCURRENTQUEUE IDENT LPAREN NUMBER RPAREN
    { Ast.DeclareQueue($2, $4) }

| IDENT DOT ENQUEUE LPAREN expr RPAREN
    { Ast.Enqueue($1, $5) }

| IDENT DOT TRY_DEQUEUE LPAREN RPAREN
    { Ast.TryDequeue($1) }

| BOOL IDENT EQUAL IDENT DOT TRY_DEQUEUE LPAREN RPAREN
    { Ast.AssignBool($2, Ast.TryDequeue($4)) }

| RETURN expr
    { Ast.Return($2) }

| IDENT EQUAL expr
    { Ast.Assign($1, $3) }


expr:
| NUMBER { IntLiteral($1) }
| IDENT  { Var($1) }
