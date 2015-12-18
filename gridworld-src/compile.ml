open Ast
open Sast


	let addTab s = print_string "\t"

	let rec print_expr (e : Sast.sexpr) = 
	match e with
	SNoexpr(_) -> print_string ""
	| SId(decl,_) ->  print_string decl
	| SInt_Lit(i,_) -> print_string (string_of_int i)
	| SString_Lit(s,_) -> print_string ("\"" ^ s ^ "\"")
  	| SBool_Lit(l,_) -> print_string(string_of_bool l)
	| SAssign(v, e,_) -> print_string (v ^ " = ") ;
		print_expr e;
	| SUniop(o,e,_) -> print_string ("!(");
		print_expr e;
		print_string ")";
	| SBinop(e1, o, e2,_) ->
	      print_expr (e1);
	      print_string (match o with
		Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
	      | Equal -> "==" | Neq -> "!="
	      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=" | Mod -> "%"
	  	  | And -> " and " | Or -> " or ");
	      print_expr(e2);
  	| SCall(f, expr_list,_) -> 
		print_string f ;
		print_string "(";
		let rec print_expr_list_comma = function
			[] -> print_string ""
			| e::[] -> print_expr e
			| e::tl -> print_expr e; print_string ", "; print_expr_list_comma tl 
			in print_expr_list_comma (List.rev expr_list); 
			print_string ")";;
	let rec print_stmt (s: Sast.sstmt)= match s with
		SExpr(e) -> (print_expr e)  :: []
	  | SPrint(e) -> 
		  print_string ("print (") ;
		  print_expr e ;
		  print_string (")"):: []
	  | SWhile(e, s) -> 
		  print_string("while (") ;
		  print_expr (e) ;
		  print_string ("):") ::
	  	(List.map addTab (List.concat (List.rev (List.map print_stmt s))))
	  | SReturn(e) -> 
	  print_string("return ");
	  print_expr e :: []
	  | SIf(e1, s1, s2) ->  
	  		match s2 with
	  		[] -> 
	  			print_string("if ");
	  			print_expr e1 ;
	  			print_string(":") :: 
	  			(List.map addTab (List.concat (List.rev (List.map print_stmt s1))));
	  		|_ -> 
	  			print_string("if ");
	  			print_expr e1;
	  			print_string(":") :: 
	  			(List.map addTab (List.concat (List.rev (List.map print_stmt s1))));
	  		 	print_string("else:") :: (List.map addTab (List.concat (List.rev (List.map print_stmt s2))));
	  |SList -> print_string("list")
	  |SChoose -> print_string("choose")
	  |SGoto(e) -> 
	  	print_expr(e);
	  	print_string("()")

 	let rec print_type (t: Sast.t)= function
	SVoid -> print_string "void ";
	| SInt -> print_string "int ";
	| SString -> print_string "String " ;
	| SBool -> print_string "boolean ";;

	let rec print_param (v: Sast.svdecl)= match v with
		|_ -> print_type v.svtype;
			print_string " ";
			print_string v.svname;;

	let rec print_param_list (p : Sast.svdecl list) = 
	match p with
		[] -> print_string "";
		| hd::[] -> print_param hd;
		| hd::tl -> print_param hd; print_string ", "; print_param_list tl;;

	let rec print_svdecl  (f : Sast.svdecl) = match f with
		|_ ->
		print_string f.svname; 
		print_string "=";
		print_expr  f.svexpr;;

	let rec print_stmt_list (p : Sast.sstmt list) = 
	match p with
		[] -> print_string "";
		| hd::[] -> print_string"";
		| hd::tl -> print_stmt hd; print_string "\n"; print_stmt_list tl;;

	let rec print_sndecl  (f : Sast.sndecl) = match f with
		|_ ->
		print_string "def ";
		print_string f.nname; 
		print_string "(";
		print_string "): \n";;
		(*List.iter print_stmt (List.rev f.sbody);;*)

	let rec print_sfdecl  (f : Sast.sfdecl) = match f with
		|_ ->
		print_string "def ";
		print_string f.fname; 
		print_string "(";
		print_param_list (List.rev f.sparams); 
		print_string "): \n";;
		(*List.iter print_stmt (List.rev f.sbody);*)
	let translate (variables, functions, nodes) =
		List.iter print_svdecl (List.rev variables);
		List.iter print_sfdecl (List.rev functions);
		List.iter print_sndecl (List.rev nodes);