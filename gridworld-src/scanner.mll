
{ open Parser } (* Get the token types *)
rule token = parse
(* Whitespace *)
[' ' '\t' '\r' '\n'] { token lexbuf } 

(* Comments *)
| "/*" 	{ comment lexbuf } 

(* Basic tokens *)
| '(' { LPAREN } | ')' { RPAREN } (* Parentheses *)
| '{' { LBRACE } | '}' { RBRACE } (* Braces *)
| '[' { LBRACKET } | ']' { RBRACKET } (* Square brackets *)
| ';' { SEMI }   | ':' { COLON } 
| ',' { COMMA }	 | '=' { ASSIGN } 

(* Arithmetic operators *)
| '+' { PLUS }   | '-' { MINUS }
| '*' { TIMES }  | '/' { DIVIDE }
| '%' { MOD }

(* Logic operators *)
| "==" { EQ }    | "!=" { NEQ }   
| '<' { LT }     | "<=" { LEQ }   
| '>' { GT }	 | ">=" { GEQ }	 
| '!' {NOT}	
| '&' {AND}		 | '|' {OR}

(* Keywords *) 		
| "if" { IF }			| "else" { ELSE } 		
| "for" { FOR }			| "elif" { ELIF }
| "function" {FUNCTION}	| "return" { RETURN }
| "break" { BREAK }		| "continue" { CONTINUE }
| "while" { WHILE }		| "node" {NODE}
| "main" {MAIN}

(* Type *)
| "int" { INT }
| "bool" { BOOL }
| "char" { CHAR }
| "string" { STRING }
(* Built-in Func *)
| "print" {PRINT}
| "list" {LIST}
| "goto" {GOTO}
| "choose" {CHOOSE}
| "readInt" {READINT}
| "readStr" {READSTR}
| "roll" {ROLL}
| eof { EOF } (* End of file *)

(* Integers *)
| ['0' - '9']+ as lxm { INT_LIT(int_of_string lxm) } 

(* Bool *)
| ("true"|"false") as boolean {BOOL_LIT(bool_of_string boolean)}

(* String *)
| '"'(['\000' - '\033' '\035' - '\127']* as str)'"' {STR_LIT(str)}

(* ID *)
| ['a' - 'z' 'A' - 'Z']['a' - 'z' 'A' - 'Z' '0' - '9' '_']* as lxm { ID(lxm) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
	"*/" { token lexbuf } (* End of comment*)
| _ { comment lexbuf } (* Eat everything else *)
