(* Abstract Syntax Tree (AST) representation of Mini-Go *)

type prog = Prog of (proc list) * stmt

and proc = Proc of string * ((exp * types) list) * (types option) * stmt

and types = TyInt
           | TyBool
           | TyChan of types
           | TyFunc of (types list * types)

and stmt = Seq of stmt * stmt
          | Go of stmt
          | Transmit of string * exp
          | RcvStmt of string
          | Decl of string * exp
          | DeclChan of string
          | Assign of string * exp
          | While of exp * stmt
          | ITE of exp * stmt * stmt
          | Return of exp
          | FuncCall of string * (exp list)
          | Print of exp

and exp = And of exp * exp
         | Eq of exp * exp
         | Gt of exp * exp
         | Plus of exp * exp
         | Minus of exp * exp
         | Times of exp * exp
         | Division of exp * exp
         | Not of exp
         | RcvExp of string
         | IConst of int
         | BConst of bool
         | Var of string
         | FuncExp of string * (exp list)


let rec print_exp exp = match exp with
  | Var s               -> String.concat "" ["Var "; s]
  | And (e1, e2)        -> String.concat "" ["("; print_exp e1; " && "; print_exp e2; ")"]
  | Eq (e1, e2)         -> String.concat "" ["("; print_exp e1; " == "; print_exp e2; ")"]
  | Gt (e1, e2)         -> String.concat "" ["("; print_exp e1; " > "; print_exp e2; ")"]
  | Plus (e1, e2)       -> String.concat "" ["("; print_exp e1; " + "; print_exp e2; ")"]
  | Minus (e1, e2)      -> String.concat "" ["("; print_exp e1; " - "; print_exp e2; ")"]
  | Times (e1, e2)      -> String.concat "" ["("; print_exp e1; " * "; print_exp e2; ")"]
  | Division (e1, e2)   -> String.concat "" ["("; print_exp e1; " / "; print_exp e2; ")"]
  | Not e               -> String.concat "" ["!("; print_exp e; ")"]
  | RcvExp s            -> String.concat "" ["(<-"; s; ")"]
  | IConst i            -> string_of_int i
  | BConst b            -> string_of_bool b
  | Var name            -> String.concat "" ["Var "; name]
  | FuncExp (name, args) -> String.concat "" [name; "("; String.concat ", " (List.map print_exp args); ")"]

let rec print_stmt stmt = match stmt with
  | Seq(s1, s2)       -> String.concat "" [print_stmt s1; ";\n"; print_stmt s2]
  | Go(s)             -> String.concat "" ["go { \n"; print_stmt s; "}"]
  | Transmit(name, e) -> String.concat "" ["Transmit("; name; "->"; print_exp e; ")"]
  | RcvStmt(name)     -> String.concat "" ["Receive(<-"; name; ")"]
  | Decl(name, e)     -> String.concat "" ["Decl("; name; " := "; print_exp e; ") "]
  | DeclChan(name)    -> String.concat "" ["DeclChan("; name; " := "; "newChannel"; ") "]
  | Assign(name, e)   -> String.concat "" ["Assign("; name; " = "; print_exp e; ") "]
  | While(e, s)       -> String.concat "" ["While(("; print_exp e; ") "; "{ "; print_stmt s; "})"]
  | ITE(e, s1, s2)    -> String.concat "" ["ITE(("; print_exp e; ")"; "{ "; print_stmt s1; "} else {"; print_stmt s2; "})"]
  | Return(e)         -> String.concat "" ["Return("; print_exp e; ")"]
  | FuncCall(name, args) -> String.concat "" ["FuncCall("; name; "("; String.concat ", " (List.map print_exp args); "))"]
  | Print(e)          -> String.concat "" ["Print("; print_exp e; ")"]

let rec print_type t = match t with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyChan types -> String.concat "" ["chan "; print_type types]
  | TyFunc (paramTypes, retType) -> String.concat "" ["func("; String.concat ", " (List.map print_type paramTypes); ") "; print_type retType]

let print_option_type t = match t with
  | None -> "void"
  | Some tp -> print_type tp

let rec print_proc p = match p with
  | Proc(name, params, retType, stmt) -> let print_param (e, t) = String.concat " " [print_exp e; print_type t]
    in let argString = String.concat ", " (List.map print_param params)
      in String.concat "" ["Proc("; name; "("; argString; ") "; print_option_type retType; " {\n"; print_stmt stmt; "})"]

let rec print_prog p = match p with
  | Prog(procs, block) -> let procsString = (String.concat "\n" (List.map (print_proc) procs))
                              in String.concat "" ["Prog("; procsString; "\n{\n\t"; print_stmt block; "\n})"]
