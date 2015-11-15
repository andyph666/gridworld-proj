type op = 
	  Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Mod

type expr =
	  Int of int
	| Bool of  bool
	| String of string
	| Id of string
	| Binop of expr * op * expr
	| Assign of string * expr
  	| Call of string * expr list
  	| Noexpr

type stmt = 
	  Print of expr
	  |If of expr * stmt * stmt
	|While of expr * stmt
	| Expr of expr
	| Return of expr



type fdecl = {
    fname : string;
    params : string list;
    body : stmt list;
	}
type program = fdecl list * stmt list