open Ast;;
open Exceptions;;

(* equality among types *)
let rec eqTy t1 t2 = match (t1,t2) with
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyChan t1, TyChan t2) -> eqTy t1 t2
  | (TyFunc (ts1, t1), TyFunc (ts2, t2)) -> eqTy t1 t2 &&
                                            (List.length ts1 == List.length ts2) &&
                                            (List.for_all (fun (t1,t2) -> eqTy t1 t2) (List.combine ts1 ts2))
  | _ -> false


(*
We assume that the type environment is represented as a list of pairs of variables and types
where variables are represented as strings.

Here's a convenience function which lookups if there's a binding for a variable.
It's actual type is actually slightly more general:
'a -> ('a * 'b) list -> 'b option

 *)
let lookup el lst = try (Some (snd (List.find (fun (el2,_) -> el = el2) lst))) with
                    | Not_found -> None

(* Update an environment with a tuple (v, t) - previous bindings for v are
overwrtirren *)
let update env binding = match binding with
  | (v, t) -> (v, t) :: List.filter (fun (v1, _) -> v <> v1) env


(* Implementation of G |- exp : t where we use 'option' to report failure *)
let rec inferTyExp env e = match e with
  | And (e1, e2)     ->   let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyBool, Some TyBool) -> Some TyBool
                          | (Some TyBool, t)           -> raise_expected_other_type TyBool t
                          | (t, _)                     -> raise_expected_other_type TyBool t)
  | Eq (e1, e2)      ->   let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyBool, Some TyBool)  -> Some TyBool
                          | (Some TyInt, Some TyInt)    -> Some TyBool
                          | (Some TyFunc _, Some TyFunc _)  -> Some TyBool
                          | (Some TyChan _, Some TyChan _)  -> Some TyBool
                          | (Some c1, Some c2)              -> raise_cannot_compare c1 c2
                          | _                               -> None)
  | Gt (e1, e2)      ->   let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyBool, Some TyBool)  -> Some TyBool
                          | (Some TyInt, Some TyInt)    -> Some TyBool
                          | (Some c1, Some c2)          -> raise_cannot_compare c1 c2
                          (* | (Some TyFunc _, Some TyFunc _)  -> Some TyBool
                          | (Some TyChan _, Some TyChan _)  -> Some TyBool *)
                          | _                           -> None)
  | Plus (e1, e2)     ->  let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyInt, Some TyInt)    -> Some TyInt
                          | (Some TyInt, t)             -> raise_expected_other_type TyInt t
                          | (t, _)                      -> raise_expected_other_type TyInt t)
  | Minus (e1, e2)    ->  let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyInt, Some TyInt)    -> Some TyInt
                          | (Some TyInt, t)             -> raise_expected_other_type TyInt t
                          | (t, _)                      -> raise_expected_other_type TyInt t)
  | Times (e1, e2)    ->  let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyInt, Some TyInt)    -> Some TyInt
                          | (Some TyInt, t)             -> raise_expected_other_type TyInt t
                          | (t, _)                      -> raise_expected_other_type TyInt t)
  | Division (e1, e2) ->  let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyInt, Some TyInt)    -> Some TyInt
                          | (Some TyInt, t)             -> raise_expected_other_type TyInt t
                          | (t, _)                      -> raise_expected_other_type TyInt t)
  | Not e             ->  (match (inferTyExp env e) with
                          | Some TyBool                 -> Some TyBool
                          | t                           -> raise_expected_other_type TyInt t)
  | RcvExp name       ->  (match (lookup name env) with
                          | Some TyChan t               -> Some t
                          | t1                          -> raise_expected_other_type (TyChan TyInt) t1)
  | IConst i          ->  Some TyInt
  | BConst b          ->  Some TyBool
  | Var name          ->  (match (lookup name env) with
                          | None                        -> raise_undecl_var name
                          | aType                       -> aType)
  | FuncExp (name,args) -> (match (lookup name env) with
                          | Some TyFunc (params, ret) -> if List.length params <> List.length args
                                                        then raise_function_count_mismatch name (List.length params) (List.length args)
                                                        else let opTyEq tyOpt ty = (match tyOpt with
                                                                                  | None    -> false
                                                                                  | Some t  -> eqTy t ty) in
                                                            let argTypes = List.map (fun t -> inferTyExp env t) args in
                                                            if (List.for_all (fun (t1, t2) -> opTyEq t1 t2) (List.combine argTypes params))
                                                            then Some ret
                                                            else raise_function_arguments_mismatch name params argTypes
                          | Some _                          -> raise_not_a_function name
                          | None                            -> raise_undecl_var name)


(* Implementation of G | (stmt : Cmd | G) where we simply skip Cmd and as above
  use 'option' to report failure

  As discussed, there's a bit of design space when it comes to 'nested' type declarations.
  The below is just a sketch.

*)
let rec typeCheckStmt env stmt = (match stmt with
  | Seq (s1, s2)  -> (match (typeCheckStmt env s1) with
                    | Some e  -> typeCheckStmt e s2  (* If s1 typechecks, typecheck s2 with the new env*)
                    | None    -> None)
  | Go s          -> (match (typeCheckStmt env s) with
                    | Some _  -> Some env (* Whaterver happens inside the block is not propagated outwith it - we return env*)
                    | None    -> None)
  | Transmit (n, e) -> (match (lookup n env) with
                    | Some (TyChan TyInt) -> (match (inferTyExp env e) with
                                    | Some TyInt  -> Some env
                                    | _           -> None)
                    | Some t       -> raise_expected_other_type (TyChan TyInt) (Some t)
                    | _            -> raise_undecl_var n) (* We only support integer channels *)
  | RcvStmt name  -> (match (lookup name env) with
                    | Some (TyChan TyInt)  -> Some env
                    | Some t       -> raise_expected_other_type (TyChan TyInt) (Some t)
                    | _            -> raise_undecl_var name) (* We only support integer channels *)
  | Decl (n, e)   -> (match (lookup n env) with
                    | Some TyFunc _ -> raise_function_redeclaration n (* We support redeclarations as long as we don't take function names *)
                    | _ -> (match (inferTyExp env e) with
                            | Some t        -> Some (update env (n, t)) (* If the expr typechecks, add the binding to the env*)
                            | None          -> None))
  (* Why not check if name is a func name *)
  | DeclChan name -> (match (lookup name env) with
                      | Some TyFunc _ -> raise_function_redeclaration name
                      | _             -> Some (update env (name, TyChan TyInt))) (* We only support integer channels *)
  | Assign (v,e)  -> (match (lookup v env) with
                    | None -> raise_undecl_var v (* Unknown variable *)
                    | Some t1 -> let t2 = inferTyExp env e in
                                 (match t2 with
                                 | None -> raise_expected_other_type t1 None
                                 | Some t3 -> if eqTy t1 t3
                                              then Some env
                                              else raise_expected_other_type t1 (Some t3)))
  | While (exp, s) -> (match (inferTyExp env exp) with
                    | Some TyBool -> (match (typeCheckStmt env s) with
                                    | Some _ -> Some env (* Disregard whatever happens inside the block *)
                                    | None   -> None)
                    | t           -> raise_expected_other_type TyBool t) (* The condition must be boolean *)
  | ITE (exp, s1, s2) -> (match (inferTyExp env exp) with
                    | Some TyBool -> (match (typeCheckStmt env s1, typeCheckStmt env s2) with
                                    | (Some _, Some _) -> Some env (* Disregard whatever happens inside the blocks *)
                                    | _                -> None)
                    | t           -> raise_expected_other_type TyBool t) (* The condition must be boolean *)
  | Return exp     -> (match (inferTyExp env exp) with
                    | Some TyFunc _ -> raise (TypeError "Cannot return a function!")
                    | Some _ -> Some env
                    | None   -> None)
  | FuncCall (n, args) -> (match (lookup n env) with
                    | Some TyFunc (params, _) ->  if List.length params <> List.length args
                                                  then raise_function_count_mismatch n (List.length params) (List.length args)
                                                  else let opTyEq tyOpt ty = (match tyOpt with
                                                                      | None    -> false
                                                                      | Some t  -> eqTy t ty) in
                                                       let argTypes = List.map (fun t -> inferTyExp env t) args in
                                                       if (List.for_all (fun (t1, t2) -> opTyEq t1 t2) (List.combine argTypes params))
                                                       then Some env
                                                       else raise_function_arguments_mismatch n params argTypes
                    | _                       -> None)
  | Print exp      -> (match (inferTyExp env exp) with
                    | Some _ -> Some env
                    | None   -> None)
  | Skip           -> Some env)

(* Is there at least one return statement? *)
let rec hasReturnStatement stmt = (match stmt with
  | Return _        -> true
  | Seq (s1, s2)    -> (hasReturnStatement s1) || (hasReturnStatement s2)
  | Go s            -> hasReturnStatement s
  | While (_, s)    -> hasReturnStatement s
  | ITE (_, s1, s2) -> (hasReturnStatement s1) || (hasReturnStatement s2)
  | _               -> false)

let rec hasReturnStatementOnThisLevel stmt = match stmt with
  | Seq (s1, s2)  -> hasReturnStatementOnThisLevel s1 || hasReturnStatementOnThisLevel s2
  | Return _      -> true
  | _             -> false

(* Returns (Some type) if there is a return statement *)
let rec inferReturnType stmt env = (match stmt with
  | Seq (s1, s2) -> (match (inferReturnType s1 env, inferReturnType s2 env) with
                | (Some t1, Some t2) -> if eqTy t1 t2
                                        then Some t1
                                        else raise (TypeError "Different paths lead to different return types")
                | (Some t, None)     -> Some t
                | (None, Some t)     -> Some t
                | _                  -> None)
  | Go s -> inferReturnType s env
  | While (_, s)    -> inferReturnType s env
  | ITE (_, s1, s2) -> (match (inferReturnType s1 env, inferReturnType s2 env) with
                | (Some t1, Some t2) -> if eqTy t1 t2
                                        then Some t1
                                        else raise (TypeError "Different paths lead to different return types")
                | (Some t, None)     -> Some t
                | (None, Some t)     -> Some t
                | _                  -> None)
  | Return exp      -> inferTyExp env exp
  | _               -> None)

(* Process a proc - return a binding of type (string * types) *)
let signatureFromProc p = match p with
  | Proc (name, [], retType, _)     -> (name, TyFunc([], retType))
  | Proc (name, params, retType, _) -> let paramTypes = List.map (fun (_, t) -> t) params in
                                       (name, TyFunc(paramTypes, retType))

(* Get all the signatures from a program - return an environment *)
let collectSignatures program = match program with
  | Prog(procs, _) -> List.map signatureFromProc procs

(* Update env with the list of expressions as long as all of them are var names *)
let rec updateEnvWithArgs env args = (match args with
  | []  -> env
  | a::rest -> (match a with
            | (Var name, t) -> updateEnvWithArgs (update env (name, t)) rest
            | (otherExp, t) -> raise_cannot_be_function_param otherExp ))

(*
   Typecheck function bodies after collecting signatures -
   we do not include anything in the function bodies in our global env,
   so this function is boolean
*)
let rec typeCheckProcs env procs = (match procs with
  | []     -> true
  | p::ps  -> (match p with
            | Proc (n, args, retType, body) -> let envWithArgs = updateEnvWithArgs env args in
                                              (match (typeCheckStmt envWithArgs body) with
                                                | Some funcEnv -> (match retType with
                                                          | TyVoid -> if hasReturnStatement body
                                                                      then raise_return_in_void_function n
                                                                      else typeCheckProcs env ps
                                                          | t      -> if not (hasReturnStatementOnThisLevel body)
                                                                      then raise_no_top_level_return_statement n
                                                                      else (match inferReturnType body funcEnv with
                                                                            | Some t1 -> if (eqTy t1 t)
                                                                                         then typeCheckProcs env ps
                                                                                         else raise_returns_wrong_type n t1 t
                                                                            | None    -> raise_no_top_level_return_statement n))
                                                | None   -> false)))
(*
 Provided with the function signatures, see if the function bodies typecheck
 and if so, typecheck the main body
*)
let typeCheckProgWithSignatures env program = match program with
  | Prog (procs, mainBody) -> if typeCheckProcs env procs
                              then typeCheckStmt env mainBody
                              else None

(* Collect signatures, perform full typecheck*)
let typeCheckProg program = let env = collectSignatures program
                            in typeCheckProgWithSignatures env program

 let rec print_env env = match env with
  | []    -> ""
  | b::bs -> (match b with
              | (n, t) -> String.concat "" [n; " -> "; print_type t; "\n"; print_env bs])
