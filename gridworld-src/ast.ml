type op = 
	  Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Mod | And | Or | Not

type scope = Local | Global

type expr =
	  Int_Lit of int
	| Bool_Lit of  bool
	| String_Lit of string
	| Id of string
	| Uniop of op * expr
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
	Int 
	| Bool 
	| String 
	| Void


type vdecl = {
    vtype : mytypes;
    vname : string;
    vexpr : expr;
}

type param_decl = 
	Param of mytypes * string

type fdecl = {
	ftype: mytypes;
    fname : string;
    params : param_decl list;
    body : stmt list;
	}

type ndecl = {
	nname: string;
	body: stmt list;
}
type program = vdecl list * fdecl list * ndecl list
