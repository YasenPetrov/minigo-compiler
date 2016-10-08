type token =
  | NAME of (string)
  | CONST_BOOL of (bool)
  | CONST_INT of (int)
  | NEWCHANNEL
  | GO
  | PRINT
  | RETURN
  | WHILE
  | IF
  | THEN
  | ELSE
  | TY_INT
  | TY_BOOL
  | TY_FUNC
  | TY_CHAN_INT
  | COMMA
  | SEMICOLON
  | AND
  | EQUALS
  | GT
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | NOT
  | LARROW
  | ASSIGN
  | INIT
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | EOL

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Ast
# 41 "parser.ml"
let yytransl_const = [|
  260 (* NEWCHANNEL *);
  261 (* GO *);
  262 (* PRINT *);
  263 (* RETURN *);
  264 (* WHILE *);
  265 (* IF *);
  266 (* THEN *);
  267 (* ELSE *);
  268 (* TY_INT *);
  269 (* TY_BOOL *);
  270 (* TY_FUNC *);
  271 (* TY_CHAN_INT *);
  272 (* COMMA *);
  273 (* SEMICOLON *);
  274 (* AND *);
  275 (* EQUALS *);
  276 (* GT *);
  277 (* PLUS *);
  278 (* MINUS *);
  279 (* TIMES *);
  280 (* DIV *);
  281 (* NOT *);
  282 (* LARROW *);
  283 (* ASSIGN *);
  284 (* INIT *);
  285 (* LPAREN *);
  286 (* RPAREN *);
  287 (* LBRACKET *);
  288 (* RBRACKET *);
  289 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* NAME *);
  258 (* CONST_BOOL *);
  259 (* CONST_INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\005\000\006\000\006\000\009\000\
\009\000\008\000\008\000\007\000\007\000\007\000\004\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\012\000\012\000\013\000\013\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\002\000\007\000\000\000\002\000\000\000\
\002\000\002\000\002\000\000\000\001\000\001\000\003\000\003\000\
\002\000\003\000\002\000\003\000\003\000\003\000\003\000\005\000\
\002\000\004\000\002\000\000\000\002\000\000\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\003\000\
\002\000\001\000\001\000\001\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\046\000\000\000\000\000\000\000\000\000\
\001\000\000\000\002\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\017\000\000\000\043\000\042\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
\000\000\015\000\010\000\011\000\000\000\000\000\007\000\000\000\
\000\000\021\000\000\000\000\000\000\000\000\000\000\000\041\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\000\000\000\000\000\013\000\014\000\000\000\009\000\000\000\
\029\000\026\000\000\000\040\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\000\031\000\045\000\024\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\011\000\007\000\023\000\070\000\024\000\
\047\000\021\000\052\000\053\000\073\000"

let yysindex = "\021\000\
\243\254\000\000\026\255\000\000\252\254\006\255\243\254\014\255\
\000\000\051\255\000\000\000\000\060\255\100\255\006\255\013\255\
\013\255\013\255\013\255\071\255\000\255\041\255\046\255\081\255\
\013\255\013\255\005\255\013\255\000\000\101\255\000\000\000\000\
\013\255\099\255\013\255\143\255\143\255\044\255\044\255\000\000\
\051\255\000\000\000\000\000\000\058\255\060\255\000\000\143\255\
\143\255\000\000\143\255\136\255\073\255\013\255\143\255\000\000\
\123\255\013\255\013\255\013\255\013\255\013\255\013\255\013\255\
\000\000\109\255\114\255\000\000\000\000\006\255\000\000\013\255\
\000\000\000\000\105\255\000\000\143\255\143\255\143\255\143\255\
\143\255\143\255\143\255\006\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\106\255\000\000\000\000\000\000\000\000\000\000\106\255\000\000\
\000\000\000\000\000\000\000\000\118\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\119\255\
\000\000\000\000\000\000\120\255\000\000\062\255\000\000\000\000\
\000\000\000\000\000\000\001\255\003\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\137\255\000\000\000\000\008\255\
\009\255\000\000\019\255\121\255\000\000\120\255\057\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\104\255\000\000\000\000\000\000\000\000\120\255\
\000\000\000\000\000\000\000\000\074\255\079\255\082\255\085\255\
\091\255\102\255\108\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\162\000\241\255\000\000\000\000\000\000\124\000\
\000\000\130\000\242\255\212\255\000\000"

let yytablesize = 171
let yytable = "\029\000\
\003\000\036\000\037\000\038\000\039\000\030\000\031\000\032\000\
\050\000\075\000\048\000\049\000\051\000\030\000\031\000\032\000\
\041\000\027\000\055\000\025\000\057\000\001\000\065\000\066\000\
\018\000\022\000\008\000\086\000\009\000\033\000\034\000\042\000\
\027\000\035\000\025\000\020\000\010\000\033\000\034\000\018\000\
\022\000\035\000\013\000\077\000\078\000\079\000\080\000\081\000\
\082\000\083\000\020\000\014\000\043\000\044\000\085\000\015\000\
\016\000\017\000\018\000\019\000\022\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\088\000\068\000\069\000\040\000\
\039\000\039\000\010\000\045\000\020\000\044\000\044\000\044\000\
\044\000\044\000\044\000\044\000\044\000\044\000\039\000\039\000\
\039\000\032\000\032\000\044\000\044\000\044\000\033\000\033\000\
\046\000\034\000\034\000\056\000\035\000\035\000\074\000\032\000\
\032\000\032\000\036\000\036\000\033\000\033\000\033\000\034\000\
\034\000\034\000\035\000\035\000\035\000\037\000\037\000\084\000\
\036\000\036\000\036\000\038\000\038\000\025\000\026\000\027\000\
\028\000\054\000\041\000\037\000\037\000\037\000\087\000\016\000\
\003\000\038\000\038\000\038\000\058\000\059\000\060\000\061\000\
\062\000\063\000\064\000\006\000\008\000\028\000\030\000\072\000\
\076\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\012\000\
\012\000\071\000\067\000"

let yycheck = "\015\000\
\014\001\016\000\017\000\018\000\019\000\001\001\002\001\003\001\
\004\001\054\000\025\000\026\000\027\000\001\001\002\001\003\001\
\017\001\017\001\033\000\017\001\035\000\001\000\038\000\039\000\
\017\001\017\001\001\001\072\000\033\001\025\001\026\001\032\001\
\032\001\029\001\032\001\017\001\031\001\025\001\026\001\032\001\
\032\001\029\001\029\001\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\032\001\001\001\012\001\013\001\070\000\005\001\
\006\001\007\001\008\001\009\001\001\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\084\000\012\001\013\001\001\001\
\016\001\017\001\031\001\030\001\026\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\030\001\031\001\
\032\001\016\001\017\001\030\001\031\001\032\001\016\001\017\001\
\016\001\016\001\017\001\001\001\016\001\017\001\030\001\030\001\
\031\001\032\001\016\001\017\001\030\001\031\001\032\001\030\001\
\031\001\032\001\030\001\031\001\032\001\016\001\017\001\011\001\
\030\001\031\001\032\001\016\001\017\001\026\001\027\001\028\001\
\029\001\029\001\017\001\030\001\031\001\032\001\030\001\032\001\
\031\001\030\001\031\001\032\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\030\001\030\001\030\001\030\001\016\001\
\030\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\031\001\
\007\000\046\000\041\000"

let yynames_const = "\
  NEWCHANNEL\000\
  GO\000\
  PRINT\000\
  RETURN\000\
  WHILE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TY_INT\000\
  TY_BOOL\000\
  TY_FUNC\000\
  TY_CHAN_INT\000\
  COMMA\000\
  SEMICOLON\000\
  AND\000\
  EQUALS\000\
  GT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  NOT\000\
  LARROW\000\
  ASSIGN\000\
  INIT\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  EOL\000\
  "

let yynames_block = "\
  NAME\000\
  CONST_BOOL\000\
  CONST_INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 25 "parser.mly"
                                  ( _1 )
# 239 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'procs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 28 "parser.mly"
                                  ( Prog (_1, _2) )
# 247 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    Obj.repr(
# 31 "parser.mly"
                                  ( [] )
# 253 "parser.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'proc) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'procs) in
    Obj.repr(
# 32 "parser.mly"
                                  ( _1 :: _2 )
# 261 "parser.ml"
               : 'procs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'retType) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 34 "parser.mly"
                                                        ( Proc( _2, _4, _6, _7 ) )
# 271 "parser.ml"
               : 'proc))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                                  ( [] )
# 277 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'param) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'param1) in
    Obj.repr(
# 38 "parser.mly"
                                  ( _1 :: _2 )
# 285 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
                                  ( [] )
# 291 "parser.ml"
               : 'param1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 42 "parser.mly"
                                  ( [_2] )
# 298 "parser.ml"
               : 'param1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 45 "parser.mly"
                              ( (Var(_1), TyInt) )
# 305 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 46 "parser.mly"
                              ( (Var(_1), TyBool) )
# 312 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                                  ( None )
# 318 "parser.ml"
               : 'retType))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                                  ( Some TyInt )
# 324 "parser.ml"
               : 'retType))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                                  ( Some TyBool )
# 330 "parser.ml"
               : 'retType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 59 "parser.mly"
                                  ( _2 )
# 337 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                                  ( Seq(_1, _3) )
# 345 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 63 "parser.mly"
                                  ( Go _2 )
# 352 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 64 "parser.mly"
                                  ( Transmit(_1, _3) )
# 360 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                                  ( RcvStmt _2 )
# 367 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 66 "parser.mly"
                                  ( Decl (_1, _3) )
# 375 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 67 "parser.mly"
                                  ( DeclChan (_1) )
# 382 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 68 "parser.mly"
                                  ( Assign (_1, _3) )
# 390 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 69 "parser.mly"
                                  ( While (_2, _3) )
# 398 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'block) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 70 "parser.mly"
                                  ( ITE (_2, _3, _5) )
# 407 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 71 "parser.mly"
                                  ( Return _2 )
# 414 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 72 "parser.mly"
                                  ( FuncCall(_1, _3) )
# 422 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 73 "parser.mly"
                                  ( Print _2 )
# 429 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                                  ( [] )
# 435 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args1) in
    Obj.repr(
# 77 "parser.mly"
                                  ( _1 :: _2 )
# 443 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                                  ( [] )
# 449 "parser.ml"
               : 'args1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 81 "parser.mly"
                                  ( _2 )
# 456 "parser.ml"
               : 'args1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 84 "parser.mly"
                                  ( And(_1, _3) )
# 464 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 85 "parser.mly"
                                  ( Eq(_1, _3) )
# 472 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 86 "parser.mly"
                                  ( Gt(_1, _3) )
# 480 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 87 "parser.mly"
                                  ( Plus(_1, _3) )
# 488 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 88 "parser.mly"
                                  ( Minus(_1, _3) )
# 496 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 89 "parser.mly"
                                  ( Times(_1, _3) )
# 504 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 90 "parser.mly"
                                  ( Division(_1, _3) )
# 512 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 91 "parser.mly"
                                  ( Not _2 )
# 519 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 92 "parser.mly"
                                  ( _2 )
# 526 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "parser.mly"
                                  ( RcvExp _2 )
# 533 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 94 "parser.mly"
                                  ( IConst _1 )
# 540 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 95 "parser.mly"
                                  ( BConst _1 )
# 547 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
                                  ( Var _1 )
# 554 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 97 "parser.mly"
                                  ( FuncExp(_1, _3) )
# 562 "parser.ml"
               : 'exp))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
