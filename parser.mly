(* parser.mly *)
%{
open Ast
%}

%token <string> IDENT
%token <int> NUMBER
%token CONCURRENTQUEUE INT BOOL VOID AUTO RETURN IF ELSE WHILE FOR NEW
%token ENQUEUE TRY_DEQUEUE ENQUEUE_BULK TRY_DEQUEUE_BULK
%token SCOPE DOT LT GT LPAREN RPAREN LBRACE RBRACE SEMICOLON COMMA EQUAL AMPERSAND ASTERISK
%token EOF

%start <Ast.t list> program
%type <Ast.t list> program

%%

program:
| stmts EOF { $1 }

stmts:
| stmt SEMICOLON stmts { $1 :: $3 }
| stmt SEMICOLON        { [$1] }

stmt:
| CONCURRENTQUEUE LT INT GT IDENT
    { DeclareQueue($5) }

| IDENT DOT ENQUEUE LPAREN expr RPAREN
    { Enqueue($1, $5) }

| IDENT DOT TRY_DEQUEUE LPAREN IDENT RPAREN
    { TryDequeue($1, $5) }

| BOOL IDENT EQUAL IDENT DOT TRY_DEQUEUE LPAREN IDENT RPAREN
    { AssignBool($2, TryDequeue($4, $7)) }

| RETURN expr
    { Return($2) }

| IDENT EQUAL expr
    { Assign($1, $3) }

expr:
| NUMBER { IntLiteral($1) }
| IDENT  { Var($1) }

