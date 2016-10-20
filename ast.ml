(* Abstract Syntax Tree (AST) representation of Mini-Go *)

type prog = Prog of (proc list) * stmt

and proc = Proc of string * ((exp * types) list) * types * stmt

and types = TyInt
           | TyBool
           | TyChan of types
           | TyFunc of (types list * types)
           | TyVoid (* Only used as a return type *)

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
          | Skip

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
  | TyVoid -> "void"

let rec print_proc p = match p with
  | Proc(name, params, retType, stmt) -> let print_param (e, t) = String.concat " " [print_exp e; print_type t]
    in let argString = String.concat ", " (List.map print_param params)
      in String.concat "" ["Proc("; name; "("; argString; ") "; print_type retType; " {\n"; print_stmt stmt; "})"]

let rec print_prog p = match p with
  | Prog(procs, block) -> let procsString = (String.concat "\n" (List.map (print_proc) procs))
                              in String.concat "" ["Prog("; procsString; "\n{\n\t"; print_stmt block; "\n})"]

(* Normalization code follows: *)

let nameSupply = ref 1
let freshName _ =  nameSupply := !nameSupply + 1;
                   String.concat "" ["temp" ; string_of_int (!nameSupply )]

let rec normalizeExp e = match e with
  | And (e1, e2) -> let r1 = normalizeExp e1 in
                    let r2 = normalizeExp e2 in
                    (Seq (fst r1, fst r2),
                     And (snd r1, snd r2))

  | Gt (e1, e2) ->  let r1 = normalizeExp e1 in
                    let r2 = normalizeExp e2 in
                    (Seq (fst r1, fst r2),
                     Gt (snd r1, snd r2))

  | Plus (e1, e2) -> let r1 = normalizeExp e1 in
                     let r2 = normalizeExp e2 in
                     (Seq (fst r1, fst r2),
                      Plus (snd r1, snd r2))

  | Minus (e1, e2) -> let r1 = normalizeExp e1 in
                      let r2 = normalizeExp e2 in
                      (Seq (fst r1, fst r2),
                       Minus (snd r1, snd r2))

  | Times (e1, e2) -> let r1 = normalizeExp e1 in
                      let r2 = normalizeExp e2 in
                      (Seq (fst r1, fst r2),
                       Times (snd r1, snd r2))

  | Division (e1, e2) -> let r1 = normalizeExp e1 in
                         let r2 = normalizeExp e2 in
                        (Seq (fst r1, fst r2),
                         Division (snd r1, snd r2))

  | Not e1            -> let r1 = normalizeExp e1 in
                         (fst r1,
                          Not (snd r1))

  | RcvExp ch         -> let x = freshName() in
                         (Decl (x, RcvExp ch),
                          Var x)

  (* Introduce Skip for convenience, maybe should have
     represented a sequence of commands as a list ... *)

  | IConst i          -> (Skip, IConst i)

  | BConst b          -> (Skip, BConst b)

  | Var x              -> (Skip, Var x)

  (* Need to normalize function arguments and make sure that order remains right *)

  | FuncExp (x,es)     -> let rs = List.map normalizeExp es in
                          let c  = List.fold_left (fun a -> fun b -> Seq (a,b)) Skip (List.map fst rs) in
                          let xs = List.map snd rs in
                          let y = freshName() in
                          (Seq (c, Decl (y, FuncExp (x,xs))),
                           Var y)

let rec normalizeStmt s = match s with
  | Seq (s1,s2) -> Seq (normalizeStmt s1, normalizeStmt s2)
  | Go s        -> Go (normalizeStmt s)
  | Transmit (x,e) -> let r = normalizeExp e in
                      Seq (fst r, Transmit (x, snd r))
  | RcvStmt x   -> RcvStmt x
  | Decl (x,e)  -> let r = normalizeExp e in
                   Seq (fst r, Decl (x, snd r))
  | DeclChan x  -> DeclChan x
  | Assign (x,e) -> let r = normalizeExp e in
                    Seq (fst r, Assign (x, snd r))
  | While (e,s)  -> let r = normalizeExp e in
                    Seq (fst r, While (snd r, normalizeStmt s))
  | ITE (e,s1,s2) -> let r = normalizeExp e in
                     Seq (fst r, ITE (snd r, normalizeStmt s1, normalizeStmt s2))
  | Return e      -> let r = normalizeExp e in
                     Seq (fst r, Return (snd r))
  | FuncCall (x, es) -> let rs = List.map normalizeExp es in
                        let c = List.fold_left (fun a -> fun b -> Seq (a,b)) Skip (List.map fst rs) in
                        let xs = List.map snd rs in
                        Seq (c, FuncCall (x,xs))
  | Print e       -> let r = normalizeExp e in
                     Seq (fst r, Print (snd r))
  | Skip          -> Skip


let normalizeProc p = match p with
    Proc (x, args, tys, s) -> Proc (x, args, tys, normalizeStmt s)


let normalizeProg p = match p with
    Prog (ps, s) -> Prog (List.map normalizeProc ps, normalizeStmt s)
