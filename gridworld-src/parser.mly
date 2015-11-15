%{ open Ast %}
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COMMA SEMI COLON ASSIGN AT
%token PLUS MINUS TIMES DIVIDE ASSIGN EXP
%token AND OR EQ NEQ LT LEQ GT GEQ 
%token RETURN IF ELSE ELIF FOR WHILE BREAK CONTINUE
%token BOOLEAN INT STRING
%token <string> TYPE
%token <int> INT_LIT
%token <string> STRING_LIT
%token <bool> BOOL_LIT
%token <string> ID
%token PRINT
%token EOF

%nonassoc ELSE COMMA
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS 
%left TIMES DIVIDE
%nonassoc INCRE DECRE
%nonassoc NEG

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
	PRINT LPAREN expr RPAREN SEMI { Print($3) }\
      INT_LIT       { Int($1) }
  	| BOOL_LIT          { Bool($1) }
  	| STRING_LIT        { String($1) }
  	| ID               { Id($1) }
	| ID ASSIGN expr   { Assign($1, $3) }
  	| ID LPAREN actuals_opt RPAREN { Call($1, $3) }
	/* bi_op */
  	| expr PLUS   expr { Binop($1, Add,   $3) }
  	| expr MINUS  expr { Binop($1, Sub,   $3) }
  	| expr TIMES  expr { Binop($1, Mult,  $3) }
  	| expr DIVIDE expr { Binop($1, Div,   $3) }
	/* bool_expr: */
	| expr AND    expr { Binary_op($1, And,   $3) }
	| expr OR     expr { Binary_op($1, Or,   $3) }
  	| expr EQ     expr { Binop($1, Equal, $3) }
  	| expr NEQ    expr { Binop($1, Neq,   $3) }
  	| expr LT     expr { Binop($1, Less,  $3) }
  	| expr LEQ    expr { Binop($1, Leq,   $3) }
  	| expr GT     expr { Binop($1, Greater,  $3) }
  	| expr GEQ    expr { Binop($1, Geq,   $3) }
  	| LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  	| actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  	| actuals_list COMMA expr { $3 :: $1 }
