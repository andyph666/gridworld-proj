type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | SEMI
  | COLON
  | GET
  | COMMA
  | ASSIGN
  | AT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | PERCENT
  | EXP
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | NOT
  | AND
  | OR
  | BREAK
  | CONTINUE
  | ELIF
  | ELSE
  | FOR
  | FUNCTION
  | RETURN
  | WHILE
  | IF
  | TYPE of (string)
  | PRINT
  | EOF
  | INT_LIT of (int)
  | BOOL_LIT of (bool)
  | STR_LIT of (string)
  | ID of (string)

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 51 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* LBRACE *);
  260 (* RBRACE *);
  261 (* LBRACKET *);
  262 (* RBRACKET *);
  263 (* SEMI *);
  264 (* COLON *);
  265 (* GET *);
  266 (* COMMA *);
  267 (* ASSIGN *);
  268 (* AT *);
  269 (* PLUS *);
  270 (* MINUS *);
  271 (* TIMES *);
  272 (* DIVIDE *);
  273 (* PERCENT *);
  274 (* EXP *);
  275 (* EQ *);
  276 (* NEQ *);
  277 (* LT *);
  278 (* LEQ *);
  279 (* GT *);
  280 (* GEQ *);
  281 (* NOT *);
  282 (* AND *);
  283 (* OR *);
  284 (* BREAK *);
  285 (* CONTINUE *);
  286 (* ELIF *);
  287 (* ELSE *);
  288 (* FOR *);
  289 (* FUNCTION *);
  290 (* RETURN *);
  291 (* WHILE *);
  292 (* IF *);
  294 (* PRINT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  293 (* TYPE *);
  295 (* INT_LIT *);
  296 (* BOOL_LIT *);
  297 (* STR_LIT *);
  298 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\005\000\006\000\006\000\
\008\000\008\000\007\000\007\000\004\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\010\000\010\000\
\011\000\011\000\000\000"

let yylen = "\002\000\
\001\000\000\000\002\000\002\000\001\000\008\000\000\000\001\000\
\003\000\005\000\000\000\002\000\005\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\003\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\035\000\000\000\000\000\000\000\003\000\004\000\
\005\000\000\000\000\000\000\000\000\000\014\000\015\000\016\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\030\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\020\000\021\000\
\000\000\000\000\000\000\000\000\000\000\000\000\009\000\011\000\
\000\000\029\000\000\000\000\000\000\000\000\000\006\000\012\000\
\010\000"

let yydgoto = "\002\000\
\003\000\004\000\007\000\008\000\009\000\020\000\060\000\021\000\
\018\000\041\000\042\000"

let yysindex = "\008\000\
\000\000\000\000\000\000\226\254\005\255\061\255\000\000\000\000\
\000\000\075\255\000\255\038\255\000\255\000\000\000\000\000\000\
\027\255\010\255\069\255\079\255\073\255\050\255\000\255\000\255\
\084\255\000\255\000\255\000\255\000\255\000\255\000\255\000\255\
\000\255\000\255\000\255\055\255\092\255\054\255\000\000\168\255\
\096\255\097\255\168\255\000\000\028\255\028\255\000\000\000\000\
\156\255\156\255\247\254\247\254\247\254\247\254\000\000\000\000\
\098\255\000\000\000\255\254\254\071\255\168\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\109\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\108\255\000\000\000\000\000\000\000\000\
\035\255\000\000\000\000\000\000\109\255\000\000\111\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\025\255\
\000\000\119\255\051\255\000\000\065\255\080\255\000\000\000\000\
\155\255\166\255\095\255\110\255\125\255\140\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\058\255\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\062\000\000\000\000\000\000\000\000\000\
\243\255\000\000\000\000"

let yytablesize = 192
let yytable = "\022\000\
\013\000\063\000\005\000\026\000\027\000\028\000\029\000\006\000\
\001\000\040\000\043\000\025\000\045\000\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\026\000\027\000\
\028\000\029\000\033\000\023\000\030\000\031\000\032\000\033\000\
\034\000\035\000\033\000\006\000\017\000\024\000\014\000\015\000\
\016\000\017\000\028\000\029\000\017\000\062\000\010\000\017\000\
\017\000\017\000\017\000\039\000\028\000\017\000\017\000\017\000\
\017\000\017\000\017\000\034\000\028\000\011\000\026\000\027\000\
\028\000\029\000\018\000\034\000\030\000\031\000\032\000\033\000\
\034\000\035\000\018\000\012\000\036\000\018\000\018\000\019\000\
\037\000\019\000\038\000\018\000\018\000\018\000\018\000\018\000\
\018\000\019\000\044\000\055\000\019\000\019\000\056\000\057\000\
\024\000\058\000\019\000\019\000\019\000\019\000\019\000\019\000\
\024\000\061\000\059\000\065\000\001\000\007\000\008\000\025\000\
\031\000\024\000\024\000\024\000\024\000\024\000\024\000\025\000\
\032\000\064\000\000\000\000\000\000\000\000\000\026\000\000\000\
\025\000\025\000\025\000\025\000\025\000\025\000\026\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\000\000\026\000\
\026\000\026\000\026\000\026\000\026\000\027\000\000\000\000\000\
\000\000\000\000\000\000\000\000\022\000\000\000\027\000\027\000\
\027\000\027\000\027\000\027\000\022\000\000\000\000\000\023\000\
\026\000\027\000\028\000\029\000\000\000\022\000\022\000\023\000\
\032\000\033\000\034\000\035\000\026\000\027\000\028\000\029\000\
\023\000\023\000\030\000\031\000\032\000\033\000\034\000\035\000"

let yycheck = "\013\000\
\001\001\004\001\033\001\013\001\014\001\015\001\016\001\038\001\
\001\000\023\000\024\000\002\001\026\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\013\001\014\001\
\015\001\016\001\002\001\001\001\019\001\020\001\021\001\022\001\
\023\001\024\001\010\001\038\001\002\001\011\001\039\001\040\001\
\041\001\042\001\015\001\016\001\010\001\059\000\042\001\013\001\
\014\001\015\001\016\001\002\001\002\001\019\001\020\001\021\001\
\022\001\023\001\024\001\002\001\010\001\001\001\013\001\014\001\
\015\001\016\001\002\001\010\001\019\001\020\001\021\001\022\001\
\023\001\024\001\010\001\001\001\008\001\013\001\014\001\042\001\
\002\001\002\001\010\001\019\001\020\001\021\001\022\001\023\001\
\024\001\010\001\007\001\037\001\013\001\014\001\003\001\042\001\
\002\001\002\001\019\001\020\001\021\001\022\001\023\001\024\001\
\010\001\008\001\010\001\037\001\000\000\002\001\002\001\002\001\
\002\001\019\001\020\001\021\001\022\001\023\001\024\001\010\001\
\002\001\060\000\255\255\255\255\255\255\255\255\002\001\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\010\001\255\255\
\255\255\255\255\255\255\255\255\255\255\002\001\255\255\019\001\
\020\001\021\001\022\001\023\001\024\001\010\001\255\255\255\255\
\255\255\255\255\255\255\255\255\002\001\255\255\019\001\020\001\
\021\001\022\001\023\001\024\001\010\001\255\255\255\255\002\001\
\013\001\014\001\015\001\016\001\255\255\019\001\020\001\010\001\
\021\001\022\001\023\001\024\001\013\001\014\001\015\001\016\001\
\019\001\020\001\019\001\020\001\021\001\022\001\023\001\024\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACKET\000\
  RBRACKET\000\
  SEMI\000\
  COLON\000\
  GET\000\
  COMMA\000\
  ASSIGN\000\
  AT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  PERCENT\000\
  EXP\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  NOT\000\
  AND\000\
  OR\000\
  BREAK\000\
  CONTINUE\000\
  ELIF\000\
  ELSE\000\
  FOR\000\
  FUNCTION\000\
  RETURN\000\
  WHILE\000\
  IF\000\
  PRINT\000\
  EOF\000\
  "

let yynames_block = "\
  TYPE\000\
  INT_LIT\000\
  BOOL_LIT\000\
  STR_LIT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bodies) in
    Obj.repr(
# 27 "parser.mly"
        ( _1 )
# 265 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "parser.mly"
                  ( [], [] )
# 271 "parser.ml"
               : 'bodies))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'bodies) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 31 "parser.mly"
                ( (_2 :: fst _1), snd _1 )
# 279 "parser.ml"
               : 'bodies))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'bodies) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 32 "parser.mly"
                ( fst _1, (_2 :: snd _1) )
# 287 "parser.ml"
               : 'bodies))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 35 "parser.mly"
       ( _1 )
# 294 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'paras_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 39 "parser.mly"
 ({
		fname = _2;
		paras = _4;
		body = List.rev _7
		})
# 307 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                    ( [] )
# 313 "parser.ml"
               : 'paras_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'paras_list) in
    Obj.repr(
# 47 "parser.mly"
                  ( List.rev _1 )
# 320 "parser.ml"
               : 'paras_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser.mly"
                                   ( [_1] )
# 328 "parser.ml"
               : 'paras_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'paras_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "parser.mly"
                                    ( _3 :: _1 )
# 337 "parser.ml"
               : 'paras_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                     ( [] )
# 343 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 55 "parser.mly"
                    ( _2 :: _1 )
# 351 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                               ( Print(_3) )
# 358 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 62 "parser.mly"
                    ( Int(_1) )
# 365 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 63 "parser.mly"
                       ( Bool(_1) )
# 372 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                    ( String(_1) )
# 379 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                      ( Id(_1) )
# 386 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                      ( Binop(_1, Add,   _3) )
# 394 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                      ( Binop(_1, Sub,   _3) )
# 402 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                      ( Binop(_1, Mult,  _3) )
# 410 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                      ( Binop(_1, Div,   _3) )
# 418 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                      ( Binop(_1, Equal, _3) )
# 426 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                      ( Binop(_1, Neq,   _3) )
# 434 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                      ( Binop(_1, Less,  _3) )
# 442 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                      ( Binop(_1, Leq,   _3) )
# 450 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                      ( Binop(_1, Greater,  _3) )
# 458 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                      ( Binop(_1, Geq,   _3) )
# 466 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                      ( Assign(_1, _3) )
# 474 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 77 "parser.mly"
                                  ( Call(_1, _3) )
# 482 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                        ( _2 )
# 489 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
                  ( [] )
# 495 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 82 "parser.mly"
                   ( List.rev _1 )
# 502 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                            ( [_1] )
# 509 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
                             ( _3 :: _1 )
# 517 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
