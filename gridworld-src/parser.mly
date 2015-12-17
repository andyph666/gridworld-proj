%{ open Ast %}
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMI COLON GET COMMA ASSIGN AT
%token PLUS MINUS TIMES DIVIDE PERCENT EXP MOD
%token EQ NEQ LT LEQ GT GEQ NOT	AND OR
%token BREAK CONTINUE ELIF ELSE FOR FUNCTION NODE RETURN WHILE IF
%token INT VOID BOOL CHAR STRING
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
%left MOD

%start program
%type <Ast.program> program
%%

program:
	/* nothing */ { [], [], [] }
| program vdecl { let (var, func, node) = $1 in $2::var, func, node }
| program fdecl { let (var, func, node) = $1 in var, $2::func, node }
| program ndecl { let (var, func, node) = $1 in var, func, $2::node }

fdecl:
	mytypes FUNCTION ID LPAREN params_opt RPAREN LBRACE stmt_list RBRACE 
	{{
    ftype = $1;
		fname = $3;
		params = $5;
		body = List.rev $8
		}}

ndecl:
  NODE ID LBRACE stmt_list RBRACE 
  {{
    nname = $2;
    body = List.rev $4
    }}

vdecl:
  mytypes ID ASSIGN expr SEMI {{ vtype = $1;
          vname = $2;
          vexpr = $4 }}   
mytypes:
  INT {Int}
  | BOOL {Bool}
  | STRING {String}
  | VOID {Void}

		  
params_opt:
      /* nothing */ { [] }
  	| params_list   { List.rev $1 }

params_list:
   	  mytypes ID               	 { [Param($1, $2)]}
  	| params_list COMMA mytypes ID { Param($3,$4)::$1 }

stmt_list:
      /* nothing */  { [] }
  	| stmt_list stmt { $2 :: $1 }

stmt:
  expr SEMI {Expr($1)}
	| PRINT LPAREN expr RPAREN SEMI { Print($3) }
	| IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE { If($3, $6, $10)}
	| WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE { While($3, $6) }
	| RETURN expr SEMI { Return($2) }

expr:
      INT_LIT       { Int_Lit($1) }
  	| BOOL_LIT          { Bool_Lit($1) }
  	| STR_LIT        { String_Lit($1) }
  	| ID               { Id($1) }
    | NOT expr    { Uniop(Not, $2) }
  	| expr PLUS   expr { Binop($1, Add,   $3) }
  	| expr MINUS  expr { Binop($1, Sub,   $3) }
  	| expr TIMES  expr { Binop($1, Mult,  $3) }
  	| expr DIVIDE expr { Binop($1, Div,   $3) }
	| expr MOD    expr { Binop($1, Mod,   $3) }
  	| expr EQ     expr { Binop($1, Equal, $3) }
  	| expr NEQ    expr { Binop($1, Neq,   $3) }
  	| expr LT     expr { Binop($1, Less,  $3) }
  	| expr LEQ    expr { Binop($1, Leq,   $3) }
  	| expr GT     expr { Binop($1, Greater,  $3) }
  	| expr GEQ    expr { Binop($1, Geq,   $3) }
    | expr AND    expr { Binop($1, And,   $3) }
    | expr OR    expr { Binop($1, Or,   $3) }
  	| ID ASSIGN expr   { Assign($1, $3) }
  	| ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  	| LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
    | actuals_list { List.rev $1 }

actuals_list:
    expr { [$1] }
    | actuals_list COMMA expr { $3 :: $1 }