open Ast
(*
let string_of_vdecl vdecl = vdecl.varname ^ match vdecl.vtype with 
| Int -> "=0"
| String -> "=\"\""
| Char -> "=\"\""
| Bool -> "=True"

let rec string_of_expr = function
	    Int_Lit(l) -> string_of_int l
	  | String_Lit(s) -> "\"" ^ s ^ "\""
	  | Bool_Lit(l) -> string_of_bool l
	  | Id(s) -> s
	  | Binop(e1, op, e2) ->
	      string_of_expr e1 ^ " " ^
	      (match op with
			Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"| Mod -> "%"
	      | Equal -> "==" | Neq -> "!="
	      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
	      string_of_expr e2
	  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
	  | Call(f, el) ->
	      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	  | Noexpr -> ""

let string_of_fullvdecl fvdecl = fvdecl.fvname ^ "=" ^ string_of_expr fvdecl.fvexpr

let rec string_of_decl = function
	string_of_vdecl vdecl
	| asdf -> string_of_fullvdecl
in
let translate_decls = function
	[] -> ""
	| hd::tl -> (string_of_decl hd) ^ "\n" ^ (translate_decls tl)
*)
(*
let translate (declarations, statements) =
	let rec string_of_expr = function
	    Int_Lit(l) -> string_of_int l
	  | String_Lit(s) -> "\"" ^ s ^ "\""
	  | Bool_Lit(l) -> string_of_bool l
	  | Id(s) -> s
	  | Binop(e1, op, e2) ->
	      string_of_expr e1 ^ " " ^
	      (match op with
			Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"| Mod -> "%"
	      | Equal -> "==" | Neq -> "!="
	      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
	      string_of_expr e2
	  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
	  | Call(f, el) ->
	      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	  | Noexpr -> ""
	in
	let rec string_of_stmt = function
		Print(expr) -> "print " ^ (string_of_expr expr)
	in
	let rec translate_stmts  = function
		  [] -> ""
		| hd::tl -> (string_of_stmt hd) ^ "\n" ^ (translate_stmts tl)
	in List.map string_of_fdecl (List.rev funcs) ^ "\n" ^ translate_stmts (List.rev statements) 
*)
let translate (declarations, statements) =
	let rec string_of_expr = function
	    Int_Lit(l) -> string_of_int l
	  | String_Lit(s) -> "\"" ^ s ^ "\""
	  | Bool_Lit(l) -> string_of_bool l
	  | Id(s) -> s
	  | Binop(e1, o, e2) ->
	      string_of_expr e1 ^ " " ^
	      (match o with
		Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
	      | Equal -> "==" | Neq -> "!="
	      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=" | Mod -> "%") ^ " " ^
	      string_of_expr e2
	  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
	  | Call(f, el) ->
	      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	  | Noexpr -> ""
	  
	in let addTab s = "\t" ^ s 
	in let rec string_of_stmt = function
		Expr(e) -> (string_of_expr e)  :: []
	  | Print(expr) -> ("print " ^ (string_of_expr expr)) :: []
	  | While(e, s) -> ("while (" ^ (string_of_expr e) ^ "):") ::
	  	(List.map addTab (List.concat (List.rev (List.map string_of_stmt s))))
	  | Return(e) -> ("return " ^ (string_of_expr e)) :: []
	  | If(e1, s1, s2) ->  
	  		match s2 with
	  		[] -> ("if " ^ (string_of_expr e1) ^ ":") :: 
	  			(List.map addTab (List.concat (List.rev (List.map string_of_stmt s1))))
	  		|_ -> (("if " ^ (string_of_expr e1) ^ ":") :: 
	  			(List.map addTab (List.concat (List.rev (List.map string_of_stmt s1))))) @ 
	  		 	("else:" :: (List.map addTab (List.concat (List.rev (List.map string_of_stmt s2)))))
	
	in let rec translate_stmts = function
		  [] -> []
		| hd::tl -> (String.concat "\n\t" (string_of_stmt hd)) :: (translate_stmts tl)

	in let string_of_vdecl vdecl =  vdecl.vname ^ "=" ^ string_of_expr vdecl.vexpr 
			^ "\n" 

	in let rec translate_vars = function
		  [] -> ""
		| hd::tl -> (string_of_vdecl hd) ^ "\n" ^ (translate_vars tl)
	in translate_vars (List.rev declarations) ^ (String.concat "\n" (translate_stmts (List.rev statements))) ^ "\n"
