{
  open Parser        (* The lexeme types are defined in parser.mli *)
  exception Eof
  open String
}
let digit = ['0'-'9']
let nonzero_digit = ['1'-'9']
let letter = (['a'-'z'] | ['A'-'Z'])
rule token = parse
    [' ' '\t']                          { token lexbuf }     (* skip blanks *)
  | ['\n' ]                             { EOL }
  | '+'                                 { PLUS }
  | '-'                                 { MINUS }
  | '*'                                 { TIMES }
  | '/'                                 { DIV }
  | '>'                                 { GT }
  | ":="                                { INIT }
  | '='                                 { ASSIGN }
  | "=="                                { EQUALS }
  | '!'                                 { NOT }
  | "&&"                                { AND }
(*| "||"                                { OR }  *) (* not currently supported*)
  | "<-"                                { LARROW }
  | '('                                 { LPAREN }
  | ')'                                 { RPAREN }
  | '{'                                 { LBRACKET }
  | '}'                                 { RBRACKET }
  | ','                                 { COMMA }
  | ';'                                 { SEMICOLON }
  | "int"                               { TY_INT }
  | "bool"                              { TY_BOOL }
  | "chan int"                          { TY_CHAN_INT }
  | "func"                              { TY_FUNC }
  | "while"                             { WHILE }
  | "if"                                { IF }
  | "then"                              { THEN }
  | "else"                              { ELSE }
  | "return"                            { RETURN }
  | "print"                             { PRINT }
  | "go"                                { GO }
  | "newChannel"                        { NEWCHANNEL }
  | "true"                              { CONST_BOOL (true) }
  | "false"                             { CONST_BOOL (false) }
  | '-'? (digit | (nonzero_digit digit*)) as i           { CONST_INT (int_of_string i)}
  | letter (letter | digit)* as name    { NAME(name) }
  (*| letter+ as name                     { FUNC_NAME(name) }*)
  | eof                                 { raise Eof }
