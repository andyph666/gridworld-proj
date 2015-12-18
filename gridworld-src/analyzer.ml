open Ast
open Sast

type symbol_table = {
	mutable parent : symbol_table option;
	mutable variables: (string * svdecl * t) list;
	mutable functions: sfdecl list;
	mutable nodes: sndecl list;
	mutable return_found: bool;
}

type environment = {
	mutable scope : symbol_table;
}

let type_expr (se : Sast.sexpr) : Sast.t = 
	match se with
		SInt_Lit(_, t) -> t
	| SBool_Lit(_,t) -> t
	| SString_Lit(_,t) -> t
	| SId(_,t) -> t
	| SUniop(_,_,t) -> t
	| SBinop(_,_,_,t) -> t
	| SAssign(_,_,t) -> t
  	| SCall(_,_,t) -> t
  	| SNoexpr(t) -> t

let rec check_id (scope : symbol_table) id =
	try
		let (_, decl, t) = List.find(fun (n, _, _) -> n = id ) scope.variables in decl, t
	with Not_found -> match scope.parent with
		Some(parent) -> check_id parent id
		| _ -> raise Not_found

let find_func (l : sfdecl list) f =
	List.find(fun c -> c.fname = f) l

let find_node (l : sndecl list) n =
	List.find(fun c -> c.nname = n) l

let rec check_expr (scope : symbol_table) (e: Ast.expr) = 
	match e with 
	Noexpr -> SNoexpr(SVoid)
	| Int_Lit(a) -> SInt_Lit(a,SInt)
	| Bool_Lit(a) -> SBool_Lit(a,SBool)
	| String_Lit(a) -> SString_Lit(a,SString)
	| Id(str) -> 	(try
						let (decl, t) = check_id scope str in SId(str, t)
					with Not_found -> raise (Failure ("Unrecognized Id " ^ str)))
	| Uniop(_,_) as u -> check_uniop scope u
	| Binop(_,_,_) as b -> check_binop scope b
	| Assign(_,_) as a -> check_assign scope a
  	| Call(_,_) as c -> check_call scope c

and check_uniop (scope : symbol_table) uniop = match uniop with
	Ast.Uniop(op, expr) -> (
		match op with
			Not ->
				let e = check_expr scope expr in
				let t = type_expr e in
				if (t <> SBool) then raise (Failure "Incorrect type for ! ") else SUniop(op, e, SBool)
			| _ -> raise (Failure "Not a uniop")
			)
	| _ -> raise (Failure "Not a uniop")

and check_binop (scope : symbol_table) binop = match binop with
	Ast.Binop(a1, op, a2) ->
		let e1 = check_expr scope a1 and e2 = check_expr scope a2 in
		let t1 = type_expr e1 and t2 = type_expr e2 in
		let t = match op with
			Add ->
				if (t1 <> SInt || t2 <> SInt) then
					if (t1 <> SString || t2 <> SString) then raise (Failure "Incorrect types for +")
					else SString
				else SInt
			| Sub -> if (t1 <> SInt || t2 <> SInt) then raise (Failure "Incorrect types for  ") else SInt
			| Mult -> if (t1 <> SInt || t2 <> SInt) then raise (Failure "Incorrect types for * ") else SInt
			| Div -> if (t1 <> SInt || t2 <> SInt) then raise (Failure "Incorrect types for / ") else SInt
			| Mod -> if (t1 <> SInt || t2 <> SInt) then raise (Failure "Incorrect types for % ") else SInt
			| Equal -> if (t1 <> t2) then raise (Failure "Incorrect types for = ") else SBool
			| Neq -> if (t1 <> t2) then raise (Failure "Incorrect types for != ") else SBool
			| Less -> if (t1 <> SInt || t2 <> SInt) then raise (Failure "Incorrect types for < ") else SBool
			| Leq -> if (t1 <> SInt || t2 <> SInt) then raise (Failure "Incorrect types for <= ") else SBool
			| Greater -> if (t1 <> SInt || t2 <> SInt) then raise (Failure "Incorrect types for > ") else SBool
			| Geq -> if (t1 <> SInt || t2 <> SInt) then raise (Failure "Incorrect types for >= ") else SBool
			| Or -> if (t1 <> SBool || t2 <> SBool) then raise (Failure "Incorrect types for | ") else SBool
			| And -> if (t1 <> SBool || t2 <> SBool) then raise (Failure "Incorrect types for & ") else SBool
			| Not -> raise (Failure "! is a unary operator.") 
		in SBinop(e1, op, e2, t)
	| _ -> raise (Failure "Not an op")

and check_assign (scope : symbol_table) a = match a with
	Ast.Assign(id, expr) ->
		let (decl, t) = check_id scope id in
		let e = check_expr scope expr in
		let t2 = type_expr e in
		if t <> t2 then raise (Failure "Incorrect type assignment.") 
		else SAssign(id, e, t)
	| _ -> raise (Failure "Not an assignment")

and check_call (scope : symbol_table) c = match c with
	Ast.Call(id, el) ->
		(try
			let f = find_func scope.functions id in
			let exprs = List.fold_left2 (fun a b c ->
					let t = b.svtype in
					let expr = check_expr scope c in
					let t2 = type_expr expr in
						if t <> t2
						then raise (Failure "wrong type")
						else expr :: a) [] f.sparams el in
						SCall(id, exprs, f.ftype)
		with 
			Not_found ->
				raise (Failure ("Function not found with name " ^ id)))
	| _ -> raise (Failure ("Not a call"))



let rec check_stmt (scope : symbol_table) (stmt : Ast.stmt) = match stmt with
	Expr(e) -> SExpr(check_expr scope e)
	| Return(e) -> SReturn(check_expr scope e)
	| If(expr, stmt1, stmt2) ->
		let new_expr = check_expr scope expr in
		let t = type_expr new_expr in
		if t <> SBool then raise (Failure "If statement must have a boolean expression")
		else
			let new_stmt1 = check_stmt_list scope stmt1 in
			let new_stmt2 = check_stmt_list scope stmt2 in
			SIf(new_expr, new_stmt1, new_stmt2)
	| While(expr, stmt) ->
		let expr = check_expr scope expr in
		let t = type_expr expr in
		if t <> SBool then raise (Failure "If statement must have a boolean expression")
		else
			let new_stmt = check_stmt_list scope stmt in
			SWhile(expr, new_stmt)
	| Print(e) -> 
		let expr = check_expr scope e in
			let t = type_expr expr in
				if (t = SString || t = SInt) then
					SPrint(expr)
				else raise (Failure "Print takes only type string or int")
	| List(e) -> 
		let exprs = List.fold_left (fun a b ->
			let expr = check_expr scope b in
			let t = type_expr expr in 
				if t <> SString then
					raise (Failure "List takes only type string")
				else expr :: a) [] e in
				SList(exprs)
	| Choose(e) ->
		let exprs = List.fold_left (fun a b ->
			let expr = check_expr scope b in
				let t = type_expr expr in
					if t <> SString then
						raise (Failure ("Choose takes only type string"))
					else 
						(try 
							let id = match b with
							 String_Lit(a) -> a
							| Id(str) -> str
							| _ -> raise (Failure "Wrong expression type in Choose") in 
							let _ = find_node scope.nodes id in
							expr :: a
						with
							Not_found ->
							raise (Failure ("Node not found")))) [] e in
					SChoose(exprs)
	| Goto(e) ->
		let expr = check_expr scope e in
			let t = type_expr expr in
				if t <> SString then
					raise (Failure ("Goto takes only type string"))
				else 
					(try 
						let id = match e with
						 String_Lit(a) -> a
						| Id(str) -> str
						| _ -> raise (Failure "Wrong expression type in Goto") in 
						let _ = find_node scope.nodes id in
						SGoto(expr)
						
					with
						Not_found ->
						raise (Failure ("Node not found")))

			
and  check_stmt_list (scope : symbol_table) (stml : Ast.stmt list) =
	List.fold_left (fun a s -> let stmt = check_stmt scope s in stmt::a) [] stml

let rec check_var_type (scope : symbol_table) (v : Ast.mytypes) = match v with
	Ast.Void -> SVoid
	| Ast.Int -> SInt
	| Ast.String -> SString
	| Ast.Bool -> SBool

let process_var_decl (scope : symbol_table) (v : Ast.vdecl) =
	let t = check_var_type scope v.vtype in
	let expr = check_expr scope v.vexpr in
	let t2 = type_expr expr in
	if t <> t2 then raise (Failure "wrong type for variable initialization") 
	else (let v={ svtype = t; svname = v.vname; svexpr = expr}
		in scope.variables <- (v.svname,v,t) :: scope.variables; v)

let rec check_func_stmt (scope : symbol_table) (stml : Sast.sstmt list) (ftype : Sast.t) =
	List.iter (fun s -> match s with
		SReturn(e) ->
			let t = type_expr e in
			if t <> ftype then raise (Failure "func return type is incorrect") else ()
		| SIf(_, s1, s2) ->
			check_func_stmt scope s1 ftype; check_func_stmt scope s2 ftype
		| SWhile(_, s) ->
			check_func_stmt scope s ftype
		| _ -> ()) stml

let rec check_node_stmt (scope : symbol_table) (stml : Sast.sstmt list) =
	List.iter (fun s -> match s with
		SIf(_, s1, s2) ->
			check_node_stmt scope s1; check_node_stmt scope s2 
		| SWhile(_, s) ->
			check_node_stmt scope s
		| _ -> ()) stml

let process_func_stmt (scope : symbol_table) (stml : Ast.stmt list) (ftype : Sast.t) =
List.fold_left (fun a s -> let stmt = check_stmt scope s in
	match stmt with
		SReturn(e) ->
			let t = type_expr e in
			if t <> ftype then raise (Failure "incorrect return type") else
			scope.return_found <- true; stmt :: a
		| SIf(_, s1, s2) ->
			check_func_stmt scope s1 ftype; check_func_stmt scope s2
			ftype; stmt :: a
		| SWhile(_, s) ->
			check_func_stmt scope s ftype; stmt :: a
		| _ -> stmt :: a) [] stml

let process_node_stmt (scope : symbol_table) (stml : Ast.stmt list)=
List.fold_left (fun a s -> let stmt = check_stmt scope s in
	match stmt with
		SReturn(e) ->
			raise (Failure "return statement in node") 
		| SIf(_, s1, s2) ->
			check_node_stmt scope s1; check_node_stmt scope s2; stmt :: a
		| SWhile(_, s) ->
			check_node_stmt scope s; stmt :: a
		| _ -> stmt :: a) [] stml

let check_func_decl (env : environment) (f : Ast.fdecl) =
	let scope' = { env.scope with parent = Some(env.scope); variables = []; nodes = [] } in
	let t = check_var_type env.scope f.ftype in
	let params = List.fold_left (fun a f -> match f with
		Ast.Param(t, n) ->
			let t = check_var_type scope' t in
			let v={ svtype = t; svname = n; svexpr = SNoexpr(SVoid)} in
			scope'.variables <- (n,v,t) :: scope'.variables; v::a) [] f.params in
	let statements = process_func_stmt scope' f.body t in
	if scope'.return_found then
		let f = { ftype = t; fname = f.fname; sparams = params; sbody = statements } in
		env.scope.functions <- f :: env.scope.functions; f
	else (if f.ftype = Void then
			let f = { ftype = t; fname = f.fname; sparams = params; sbody = statements } in
			env.scope.functions <- f :: env.scope.functions; f
		else raise (Failure ("No return for function " ^ f.fname ^ " when return expected.")))


let check_node_decl (env : environment) (n : Ast.ndecl) =
	let scope' = { env.scope with parent = Some(env.scope); variables = []; functions = [] } in
	let statements = process_node_stmt scope' n.body in
	let n = { nname = n.nname; sbody = statements } in
	env.scope.nodes <- n :: env.scope.nodes; n

let process_func_decl (env : environment) (f : Ast.fdecl) =
	try
		let _ = find_func env.scope.functions f.fname in
			raise (Failure ("Function already declared with name " ^ f.fname))
	with Not_found ->
		if (f.fname = "print" || f.fname = "goto" || f.fname = "list" || f.fname = "choose" || f.fname = "main") 
		then raise (Failure "A function cannot have same name as built-in function")
		else
			check_func_decl env f

let process_node_decl (env : environment) (n : Ast.ndecl) =
	try
		let _ = find_func env.scope.functions n.nname in
			raise (Failure ("Node with same name as function " ^ n.nname))
	with Not_found ->
		if (n.nname = "print" || n.nname= "goto" || n.nname = "list" || n.nname = "choose" || n.nname = "main") 
		then raise (Failure "A node cannot have same name as built-in function")
		else
			try
				let _ = find_node env.scope.nodes n.nname in
					raise (Failure ("Node already declared with name  " ^ n.nname))
			with Not_found ->
				check_node_decl env n

let process_global_decl (env : environment) (g : Ast.vdecl) =
	try
		let _ = check_id env.scope g.vname in 
		raise (Failure ("Variable already declared with name " ^ g.vname))
	with Not_found ->
	process_var_decl env.scope g


let check_program (p : Ast.program) =
	let s = { parent = None; variables = []; functions = []; nodes = []; return_found = false} in
	let env = { scope = s } in
	let (vars, funcs, nodes) = p in
	let globals = List.fold_left (fun a g -> process_global_decl env g :: a) [] (List.rev vars) in
	let funcs = List.fold_left (fun a f -> process_func_decl env f :: a) [] (List.rev funcs) in
	let nodes = List.fold_left (fun a n -> process_node_decl env n :: a) [] nodes in
	globals, funcs, nodes
