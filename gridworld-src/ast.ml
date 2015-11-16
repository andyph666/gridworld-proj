type op = 
	  Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Mod

type expr =
	  Int_Lit of int
	| Bool_Lit of  bool
	| String_Lit of string
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

type mytypes = Int 

type vdecl = {
    varname : string;
    vartype : mytypes; 
}

type fullvdecl = {
    fvtype : mytypes;
    fvname : string;
    fvexpr : expr;
}

type fdecl = {
    fname : string;
    params : string list;
    body : stmt list;
	}
type program = fdecl list * stmt list