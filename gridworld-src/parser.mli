type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | SEMI
  | COLON
  | GET
  | COMMA
  | ASSIGN
  | AT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | PERCENT
  | EXP
  | MOD
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | NOT
  | AND
  | OR
  | BREAK
  | CONTINUE
  | ELIF
  | ELSE
  | FOR
  | FUNCTION
  | RETURN
  | WHILE
  | IF
  | TYPE of (string)
  | PRINT
  | EOF
  | INT_LIT of (int)
  | BOOL_LIT of (bool)
  | STR_LIT of (string)
  | ID of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
