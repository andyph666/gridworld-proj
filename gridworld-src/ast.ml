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
	| If of expr * stmt list * stmt list
	| While of expr * stmt list
	| Expr of expr
	| Return of expr

type mytypes = 
	| Int
	| Bool 
	| String
	

type vdecl = {
    vtype : mytypes;
    vname : string;
    vexpr : expr;
}

type fdecl = {
    fname : string;
    params : string list;
    body : stmt list;
	}
type program = vdecl list * stmt list