%{
  open Ast
%}
  %token <string> NAME
  %token <bool> CONST_BOOL
  %token <int> CONST_INT
  %token NEWCHANNEL GO PRINT RETURN WHILE IF THEN ELSE TY_INT TY_BOOL TY_FUNC TY_CHAN_INT
  %token COMMA SEMICOLON
  %right LARROW
  %left EQUALS
  %nonassoc AND
  %left GT
  %right NOT
  %left PLUS MINUS
  %left TIMES
  %right DIV
  %token LARROW EQUALS GT AND PLUS MINUS DIV TIMES NOT
  %token ASSIGN
  %token INIT
  %token LPAREN RPAREN
  %token LBRACKET RBRACKET
  %token EOL
  %start main             /* the entry point */
  %type <Ast.prog> main
  %%
  main:
      prog EOL                    { $1 }
  ;
  prog:
      procs block                 { Prog ($1, $2) }
  ;
  procs:
      |                           { [] }
      | proc procs                { $1 :: $2 }
  proc:
      TY_FUNC NAME LPAREN params RPAREN retType block   { Proc( $2, $4, $6, (Locals [], $7) ) }
  ;
  params:
      |                           { [] }
      | param param1              { $1 :: $2 }
  ;
  param1:
      |                           { [] }
      | COMMA param               { [$2] }
  ;
  param:
      | NAME TY_INT           { (Var($1), TyInt) }
      | NAME TY_BOOL          { (Var($1), TyBool) }
      /*| NAME funcSign         { ($1, $2) }  Can't know the signature of this function when it's passed as an argument */
  ;
  /*funcSign:
      TY_FUNC LPAREN params RPAREN retType   { TyFunc($3, $5) }
  ;*/
  retType:
      |                           { TyVoid }
      | TY_INT                    { TyInt }
      | TY_BOOL                   { TyBool }
      /*| funcSign                  { Some $1 }*/
  ;
  block:
      LBRACKET stmt RBRACKET      { $2 }
  ;
  stmt:
      | stmt SEMICOLON stmt       { Seq($1, $3) }
      | GO block                  { Go $2 }
      | NAME LARROW exp           { Transmit($1, $3) }
      | LARROW NAME               { RcvStmt $2 }
      | NAME INIT exp             { Decl ($1, $3) }
      | NAME INIT NEWCHANNEL      { DeclChan ($1) }
      | NAME ASSIGN exp           { Assign ($1, $3) }
      | WHILE exp block           { While ($2, (Locals [], $3)) }
      | IF exp block ELSE block   { ITE ($2, (Locals [], $3), (Locals [], $5)) }
      | RETURN exp                { Return $2 }
      | NAME LPAREN args RPAREN   { FuncCall($1, $3) }
      | PRINT exp                 { Print $2 }
  ;
  args:
      |                           { [] }
      | exp args1                 { $1 :: $2 }
  ;
  args1:
      |                           { [] }
      | COMMA args                { $2 }
  ;
  exp:
      | exp AND exp               { And($1, $3) }
      | exp EQUALS exp            { Eq($1, $3) }
      | exp GT exp                { Gt($1, $3) }
      | exp PLUS exp              { Plus($1, $3) }
      | exp MINUS exp             { Minus($1, $3) }
      | exp TIMES exp             { Times($1, $3) }
      | exp DIV exp               { Division($1, $3) }
      | NOT exp                   { Not $2 }
      | LPAREN exp RPAREN         { $2 }
      | LARROW NAME               { RcvExp $2 }
      | CONST_INT                 { IConst $1 }
      | CONST_BOOL                { BConst $1 }
      | NAME                      { Var $1 }
      | NAME LPAREN args RPAREN   { FuncExp($1, $3) }
  ;
  /*bexp:
      | cexp bexp1                { And()}
  ;
  aexp:
  ;
  aexp1:
  ;
  term:
  ;
  term1:
  ;
  factor:
      | CONST_INT                 { IConst $1 }
      | CONST_BOOL                { BConst $2 }
      | NAME                  { Var $1 }
      | NOT factor                { Not $2 }
      | LPAREN bexp RPAREN        { $2 }
      | NAME LPAREN args RPAREN { FuncExp($1, $3) }
  bexp:
      | LPAREN { Var "a" }
  ;
  aexp:
      | LPAREN { Var "a" }
  ;*/
