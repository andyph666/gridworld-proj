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
  | MOD
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
# 52 "parser.ml"
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
  275 (* MOD *);
  276 (* EQ *);
  277 (* NEQ *);
  278 (* LT *);
  279 (* LEQ *);
  280 (* GT *);
  281 (* GEQ *);
  282 (* NOT *);
  283 (* AND *);
  284 (* OR *);
  285 (* BREAK *);
  286 (* CONTINUE *);
  287 (* ELIF *);
  288 (* ELSE *);
  289 (* FOR *);
  290 (* FUNCTION *);
  291 (* RETURN *);
  292 (* WHILE *);
  293 (* IF *);
  295 (* PRINT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  294 (* TYPE *);
  296 (* INT_LIT *);
  297 (* BOOL_LIT *);
  298 (* STR_LIT *);
  299 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\004\000\005\000\005\000\007\000\
\007\000\006\000\006\000\003\000\003\000\003\000\003\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\009\000\009\000\010\000\010\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\001\000\008\000\000\000\001\000\003\000\
\005\000\000\000\002\000\005\000\007\000\005\000\003\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\003\000\004\000\000\000\000\000\016\000\017\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\033\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\024\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\032\000\000\000\014\000\000\000\012\000\008\000\010\000\
\000\000\000\000\000\000\000\000\000\000\013\000\005\000\011\000\
\009\000"

let yydgoto = "\002\000\
\003\000\009\000\010\000\011\000\042\000\076\000\043\000\018\000\
\046\000\047\000"

let yysindex = "\003\000\
\000\000\000\000\040\000\218\254\000\255\008\255\009\255\031\255\
\000\000\000\000\000\000\047\255\000\255\000\000\000\000\000\000\
\002\255\022\000\000\255\000\255\000\255\244\254\100\255\000\255\
\000\255\000\000\000\255\000\255\000\255\000\255\000\255\000\255\
\000\255\000\255\000\255\000\255\000\255\115\255\130\255\145\255\
\058\255\053\255\057\255\000\000\035\000\067\255\060\255\035\000\
\030\255\030\255\054\255\054\255\000\000\048\000\048\000\159\255\
\159\255\159\255\159\255\246\254\246\254\065\255\040\255\087\255\
\046\255\000\000\000\255\000\000\059\255\000\000\000\000\000\000\
\089\255\035\000\246\254\254\254\056\255\000\000\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\103\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\037\255\000\000\000\000\000\000\000\000\102\255\000\000\109\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\110\255\000\000\026\255\000\000\116\255\086\255\
\169\255\193\255\061\255\085\255\000\000\178\255\007\000\202\255\
\226\255\232\255\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\028\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\243\255\
\000\000\000\000"

let yytablesize = 335
let yytable = "\023\000\
\013\000\079\000\024\000\001\000\012\000\038\000\039\000\040\000\
\019\000\020\000\045\000\048\000\025\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\005\000\006\000\007\000\036\000\008\000\037\000\041\000\021\000\
\005\000\006\000\007\000\036\000\008\000\037\000\019\000\014\000\
\015\000\016\000\017\000\019\000\029\000\030\000\019\000\022\000\
\031\000\019\000\019\000\019\000\019\000\074\000\064\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\022\000\068\000\
\069\000\063\000\065\000\022\000\066\000\067\000\022\000\070\000\
\031\000\022\000\022\000\022\000\022\000\071\000\078\000\080\000\
\022\000\022\000\022\000\022\000\022\000\022\000\023\000\031\000\
\073\000\072\000\075\000\023\000\031\000\081\000\023\000\031\000\
\077\000\023\000\023\000\023\000\023\000\044\000\038\000\006\000\
\023\000\023\000\023\000\023\000\023\000\023\000\034\000\007\000\
\027\000\028\000\029\000\030\000\060\000\035\000\031\000\032\000\
\033\000\034\000\035\000\036\000\037\000\000\000\000\000\027\000\
\028\000\029\000\030\000\061\000\000\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\000\000\000\000\027\000\028\000\
\029\000\030\000\062\000\000\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\000\000\000\000\027\000\028\000\029\000\
\030\000\000\000\000\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000\020\000\027\000\028\000\029\000\030\000\020\000\
\000\000\031\000\020\000\025\000\000\000\020\000\020\000\000\000\
\025\000\000\000\000\000\025\000\020\000\020\000\020\000\020\000\
\020\000\020\000\021\000\000\000\000\000\025\000\025\000\021\000\
\000\000\000\000\021\000\027\000\000\000\021\000\021\000\000\000\
\027\000\000\000\000\000\027\000\021\000\021\000\021\000\021\000\
\021\000\021\000\000\000\000\000\000\000\027\000\027\000\027\000\
\027\000\027\000\027\000\028\000\000\000\000\000\000\000\000\000\
\028\000\029\000\000\000\028\000\000\000\000\000\029\000\000\000\
\000\000\029\000\000\000\000\000\000\000\028\000\028\000\028\000\
\028\000\028\000\028\000\029\000\029\000\029\000\029\000\029\000\
\029\000\000\000\030\000\000\000\000\000\000\000\000\000\030\000\
\026\000\000\000\030\000\000\000\000\000\026\000\000\000\000\000\
\026\000\000\000\000\000\000\000\030\000\030\000\030\000\030\000\
\030\000\030\000\026\000\026\000\026\000\000\000\000\000\000\000\
\000\000\000\000\027\000\028\000\029\000\030\000\000\000\000\000\
\031\000\032\000\033\000\034\000\035\000\036\000\037\000\027\000\
\028\000\029\000\030\000\000\000\000\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\027\000\028\000\029\000\030\000\
\000\000\000\000\031\000\000\000\000\000\034\000\035\000\036\000\
\037\000\004\000\005\000\006\000\007\000\000\000\008\000"

let yycheck = "\013\000\
\001\001\004\001\001\001\001\000\043\001\019\000\020\000\021\000\
\001\001\001\001\024\000\025\000\011\001\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\035\001\036\001\037\001\002\001\039\001\002\001\043\001\001\001\
\035\001\036\001\037\001\010\001\039\001\010\001\002\001\040\001\
\041\001\042\001\043\001\007\001\015\001\016\001\010\001\001\001\
\019\001\013\001\014\001\015\001\016\001\067\000\002\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\002\001\060\000\
\061\000\008\001\010\001\007\001\002\001\010\001\010\001\007\001\
\019\001\013\001\014\001\015\001\016\001\038\001\075\000\076\000\
\020\001\021\001\022\001\023\001\024\001\025\001\002\001\002\001\
\043\001\003\001\032\001\007\001\007\001\038\001\010\001\010\001\
\008\001\013\001\014\001\015\001\016\001\002\001\000\000\002\001\
\020\001\021\001\022\001\023\001\024\001\025\001\002\001\002\001\
\013\001\014\001\015\001\016\001\002\001\002\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\255\255\255\255\013\001\
\014\001\015\001\016\001\002\001\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\255\255\255\255\013\001\014\001\
\015\001\016\001\002\001\255\255\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\255\255\255\255\013\001\014\001\015\001\
\016\001\255\255\255\255\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\002\001\013\001\014\001\015\001\016\001\007\001\
\255\255\019\001\010\001\002\001\255\255\013\001\014\001\255\255\
\007\001\255\255\255\255\010\001\020\001\021\001\022\001\023\001\
\024\001\025\001\002\001\255\255\255\255\020\001\021\001\007\001\
\255\255\255\255\010\001\002\001\255\255\013\001\014\001\255\255\
\007\001\255\255\255\255\010\001\020\001\021\001\022\001\023\001\
\024\001\025\001\255\255\255\255\255\255\020\001\021\001\022\001\
\023\001\024\001\025\001\002\001\255\255\255\255\255\255\255\255\
\007\001\002\001\255\255\010\001\255\255\255\255\007\001\255\255\
\255\255\010\001\255\255\255\255\255\255\020\001\021\001\022\001\
\023\001\024\001\025\001\020\001\021\001\022\001\023\001\024\001\
\025\001\255\255\002\001\255\255\255\255\255\255\255\255\007\001\
\002\001\255\255\010\001\255\255\255\255\007\001\255\255\255\255\
\010\001\255\255\255\255\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\020\001\021\001\007\001\255\255\255\255\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\255\255\255\255\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\013\001\
\014\001\015\001\016\001\255\255\255\255\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\013\001\014\001\015\001\016\001\
\255\255\255\255\019\001\255\255\255\255\022\001\023\001\024\001\
\025\001\034\001\035\001\036\001\037\001\255\255\039\001"

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
  MOD\000\
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
    Obj.repr(
# 28 "parser.mly"
               ( [], [] )
# 309 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 29 "parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 317 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 30 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 325 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 33 "parser.mly"
       ( _1 )
# 332 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'params_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 37 "parser.mly"
 ({
		fname = _2;
		params = _4;
		body = List.rev _7
		})
# 345 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
                    ( [] )
# 351 "parser.ml"
               : 'params_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'params_list) in
    Obj.repr(
# 46 "parser.mly"
                   ( List.rev _1 )
# 358 "parser.ml"
               : 'params_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "parser.mly"
                                   ( [_1] )
# 366 "parser.ml"
               : 'params_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'params_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser.mly"
                                     ( _3 :: _1 )
# 375 "parser.ml"
               : 'params_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                     ( [] )
# 381 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 54 "parser.mly"
                    ( _2 :: _1 )
# 389 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                               ( Print(_3) )
# 396 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 58 "parser.mly"
                                        ( If(_3, _5, _7))
# 405 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 59 "parser.mly"
                                 ( While(_3, _5) )
# 413 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                    ( Return(_2) )
# 420 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 63 "parser.mly"
                    ( Int(_1) )
# 427 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 64 "parser.mly"
                       ( Bool(_1) )
# 434 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                    ( String(_1) )
# 441 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                      ( Id(_1) )
# 448 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                      ( Binop(_1, Add,   _3) )
# 456 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                      ( Binop(_1, Sub,   _3) )
# 464 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                      ( Binop(_1, Mult,  _3) )
# 472 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                      ( Binop(_1, Div,   _3) )
# 480 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                    ( Binop(_1, Mod,   _3) )
# 488 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                      ( Binop(_1, Equal, _3) )
# 496 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                      ( Binop(_1, Neq,   _3) )
# 504 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                      ( Binop(_1, Less,  _3) )
# 512 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                      ( Binop(_1, Leq,   _3) )
# 520 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                      ( Binop(_1, Greater,  _3) )
# 528 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                      ( Binop(_1, Geq,   _3) )
# 536 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                      ( Assign(_1, _3) )
# 544 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 79 "parser.mly"
                                  ( Call(_1, _3) )
# 552 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                        ( _2 )
# 559 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                  ( [] )
# 565 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 84 "parser.mly"
                   ( List.rev _1 )
# 572 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                            ( [_1] )
# 579 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
                             ( _3 :: _1 )
# 587 "parser.ml"
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
