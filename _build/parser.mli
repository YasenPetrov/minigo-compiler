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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog
