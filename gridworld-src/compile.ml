open Ast
open Sast


	let addTab s = s^"\t"
	let range a b =
	  	let rec aux a b =
      		if a > b then [] else a :: aux (a+1) b  in
		if a > b then List.rev (aux b a) else aux a b;;

	let rec print_list = function 
		[] -> ()
		| e::l -> print_int e ; print_string " "; print_list l;;

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
		  	  | And -> " and " | Or -> " or "|_ ->"");
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
	let rec print_expr_noquote (e : Sast.sexpr) = 
		match e with
		| SString_Lit(s,_) -> print_string ( s );
		| SId(decl,_) ->  print_string decl;
	  	| _ -> print_string"";;
	let rec print_stmt (s: Sast.sstmt) (tab:string)= match s with
		SExpr(e) -> print_string tab;(print_expr e); print_string"\n";
	 | SPrint(e) ->
		 print_string tab;print_string ("print (") ;
		 print_expr e ;
		 print_string (")\n")
	 | SWhile(e, s) ->
		 print_string tab;print_string("while (") ;
		 print_expr (e) ;
		 print_string ("):\n") ;
		 print_string tab;
		 print_stmt_wTab s (addTab tab);
	  	print_string "\n"
	 | SReturn(e) ->
	 	print_string tab;print_string("return ");
	 	print_expr e
	 |SList(e) ->
	 	print_string tab;
	 	print_string "print(\"\\n";
	  	List.iter2 (fun a b-> (print_int a;print_string ": "; print_expr_noquote b;print_string"\\n")) (range 1 (List.length(e))) (List.rev e);
	  	print_string "\")\n"
	 |SChoose(e) -> 
	 	print_string tab;
	 	print_string"choice = int(input(\"Enter a choice: \"))\n";
	 	print_string tab;print_string "\t";
	 	print_string "while(choice!=-1):\n";
	 	List.iter2 (fun a b-> (print_string tab;print_string "\t\tif (choice==";print_int a;print_string "):\n"; print_string tab;print_string "\t\t\t"; print_expr b;print_string"()\n")) (range 1 (List.length(e))) (List.rev e);
	 	print_string tab;print_string "\t\telse:\n";print_string tab;print_string"\t\t\tchoice = int(input(\"Invalid Input! Please Re-enter: \"))\n";
	 |SGoto(e) ->
	  	print_string tab;print_expr e; print_string"()\n";
	 |SReadInt(e) ->
	 	print_string tab;
	 	print_expr e;
	 	print_string " = int(raw_input());\n"
	 |SReadStr(e) ->
	 	print_string tab;
	 	print_expr e;
	 	print_string " = str(raw_input());\n"
	 |SRoll(e) ->
	 	print_string tab;
	 	print_expr e;
	 	print_string " = randint(1,6);"
	 | SIf(e1, s1, s2) ->
	  		match s2 with
	  		[] ->
	  			print_string tab;
	  			print_string("if ");
	  			print_expr e1 ;
	  			print_string(":\n");
	  			print_stmt_wTab s1 (addTab tab);
	  			print_string("")
	  		|_ ->
	  			print_string tab;
	  			print_string("if ");
	  			print_expr e1;
	  			print_string(":\n");
	  			print_stmt_wTab s1 (addTab tab);
	  		 	print_string ("\n");
	  		 	print_string tab;
	  		 	print_string("\telse:\n");
	  		 	print_stmt_wTab s2 (addTab tab);
	  		 	print_string ""
	and print_stmt_wTab (s:Sast.sstmt list) (tab:string) = match s with 
		[] -> print_string "";
		| hd::[] ->	 print_string tab;print_stmt hd tab;
		| hd::tl -> print_string tab ;print_stmt hd tab;print_stmt_wTab tl tab;;
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
		print_expr  f.svexpr;
		print_string "\n";;

	let rec print_stmt_list (p : Sast.sstmt list) = 
	match p with
		[] -> print_string "";
		| hd::[] ->	print_string "\t";print_stmt hd ""; print_string "\n";
		| hd::tl -> print_string "\t";print_stmt hd ""; print_string "\n"; print_stmt_list tl;;

	let rec print_sndecl  (f : Sast.sndecl list)(v: Sast.svdecl list) = match f with
		[] -> print_string "";
		| hd::[] ->	
			print_string "def ";
			print_string hd.nname; 
			print_string "(";
			print_string "):";
			print_globals v;
			print_stmt_list (List.rev hd.sbody);
			print_string "\texit()\n";
		| hd::tl -> 
			print_string "def ";
			print_string hd.nname; 
			print_string "(";
			print_string "):";
			print_globals v;
			print_stmt_list (List.rev hd.sbody);
			print_string "\texit()\n";
			print_sndecl tl v;
			print_string "";
		(*List.iter print_stmt (List.rev f.sbody);*)
	and print_globals (v:Sast.svdecl list) = match v with
		[] -> print_string "";
		| hd::[] ->	print_string("\n\tglobal "); print_string hd.svname; print_string ";\n";
		| hd::tl -> print_string("\n\tglobal "); print_string hd.svname; print_string ";"; print_globals tl;;
	let rec print_sfdecl  (f : Sast.sfdecl list)(v: Sast.svdecl list) = match f with
		[] -> print_string "";
		| hd::[] ->	
			print_string "def ";
			print_string hd.fname; 
			print_string "(";
			print_param_list (List.rev hd.sparams);
			print_string "):";
			print_globals v;
			print_stmt_list (List.rev hd.sbody);
			print_string "\texit()\n";
		| hd::tl -> 
			print_string "def ";
			print_string hd.fname; 
			print_string "(";
			print_param_list (List.rev hd.sparams);
			print_string "):";
			print_globals v;
			print_stmt_list (List.rev hd.sbody);
			print_string "\texit()\n";
			print_sfdecl tl v;
			print_string "";;
	let translate (variables, functions, nodes) =
		print_string "from random import randint\n";
		List.iter print_svdecl (List.rev variables);
		print_sfdecl (List.rev functions) variables;
		print_sndecl (List.rev nodes) variables;
		print_string "if __name__ == '__main__':\n\tmain()";