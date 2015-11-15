%{ open Ast %}
%token LPAREN RPAREN LBRACE RBRACE LSQUAR RSQUAR SEMI COLON GET COMMA ASSIGN AT
%token PLUS MINUS TIMES DIVIDE PERCENT EXP
%token EQ NEQ LT LEQ GT GEQ NOT	AND OR
%token BREAK CONTINUE ELIF ELSE FOR FUNCTION RETURN WHILE IF
%token <string> TYPE
%token PRINT
%token EOF

%token <int> INT_LIT
%token <bool> BOOL_LIT
%token <string> STR_LIT
%token <string> ID


%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program
%%

program:
	/* nothing */ { [], [] }
 	| program decl { ($2 :: fst $1), snd $1 }
 	| program stmt { fst $1, ($2 :: snd $1) }

decl:
	fdecl { $1 }

fdecl:
	FUNCTION ID LPAREN params_opt RPAREN LBRACE stmt_list RBRACE 
	{{
		fname = $2;
		params = $4;
		body = List.rev $7
		}}

params_opt:
      /* nothing */ { [] }
  	| params_list   { List.rev $1 }

params_list:
   	  ID COLON TYPE              	 { [$1] }
  	| params_list COMMA ID COLON TYPE { $3 :: $1 }

stmt_list:
      /* nothing */  { [] }
  	| stmt_list stmt { $2 :: $1 }

stmt:
	PRINT LPAREN expr RPAREN SEMI { Print($3) }
	| IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7)}
	| WHILE LPAREN expr RPAREN stmt { While($3, $5) }
	| BREAK SEMI  { Break }
	| CONTINUE SEMI  { Continue }
	| RETURN expr SEMI { Return($2) }

expr:
      INT_LIT       { Int($1) }
  	| BOOL_LIT          { Bool($1) }
  	| STR_LIT        { String($1) }
  	| ID               { Id($1) }
  	| expr PLUS   expr { Binop($1, Add,   $3) }
  	| expr MINUS  expr { Binop($1, Sub,   $3) }
  	| expr TIMES  expr { Binop($1, Mult,  $3) }
  	| expr DIVIDE expr { Binop($1, Div,   $3) }
  	| expr EQ     expr { Binop($1, Equal, $3) }
  	| expr NEQ    expr { Binop($1, Neq,   $3) }
  	| expr LT     expr { Binop($1, Less,  $3) }
  	| expr LEQ    expr { Binop($1, Leq,   $3) }
  	| expr GT     expr { Binop($1, Greater,  $3) }
  	| expr GEQ    expr { Binop($1, Geq,   $3) }
  	| ID ASSIGN expr   { Assign($1, $3) }
  	| ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  	| LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  	| actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  	| actuals_list COMMA expr { $3 :: $1 }
