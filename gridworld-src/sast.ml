open Ast
type t =
	SInt
	| SString
	| SBool
	| SVoid

type sexpr =
	  SInt_Lit of int * t
	| SBool_Lit of  bool * t
	| SString_Lit of string * t
	| SId of string * t
	| SUniop of op * sexpr * t
	| SBinop of sexpr * Ast.op * sexpr * t
	| SAssign of string * sexpr * t
  	| SCall of string * sexpr list * t
  	| SNoexpr of t

type sstmt = 
	SPrint of sexpr
	| SList of sexpr list
	| SChoose of sexpr list
	| SGoto of sexpr
	| SIf of sexpr * sstmt list * sstmt list
	| SWhile of sexpr * sstmt list
	| SExpr of sexpr
	| SReturn of sexpr
	| SReadInt of sexpr
	| SReadStr of sexpr


type svdecl = {
    svtype : t;
    svname : string;
    svexpr : sexpr;
}

type sfdecl = {
	ftype : t;
    fname : string;
    sparams : svdecl list;
    sbody : sstmt list;
}

type sndecl = {
	nname : string;
	sbody : sstmt list;
}