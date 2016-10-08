open Ast;;

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
  | (v, t) -> (v, t) :: List.filter (fun (v1, _) -> v1 <> v1) env


(* Implementation of G |- exp : t where we use 'option' to report failure *)
let rec inferTyExp env e = match e with
  | And (e1, e2)     ->   let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyBool, Some TyBool) -> Some TyBool
                          | _                          -> None)
  | Eq (e1, e2)      ->   let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyBool, Some TyBool)  -> Some TyBool
                          | (Some TyInt, Some TyInt)    -> Some TyBool
                          | (Some TyFunc _, Some TyFunc _)  -> Some TyBool
                          | (Some TyChan _, Some TyChan _)  -> Some TyBool
                          | _                           -> None)
  | Gt (e1, e2)      ->   let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyBool, Some TyBool)  -> Some TyBool
                          | (Some TyInt, Some TyInt)    -> Some TyBool
                          (* | (Some TyFunc _, Some TyFunc _)  -> Some TyBool
                          | (Some TyChan _, Some TyChan _)  -> Some TyBool *)
                          | _                           -> None)
  | Plus (e1, e2)     ->  let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyInt, Some TyInt)    -> Some TyInt
                          | _                           -> None)
  | Minus (e1, e2)    ->  let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyInt, Some TyInt)    -> Some TyInt
                          | _                           -> None)
  | Times (e1, e2)    ->  let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyInt, Some TyInt)    -> Some TyInt
                          | _                           -> None)
  | Division (e1, e2) ->  let (t1, t2) = (inferTyExp env e1, inferTyExp env e2) in
                          (match (t1, t2) with
                          | (Some TyInt, Some TyInt)    -> Some TyInt
                          | _                           -> None)
  | Not e             ->  (match (inferTyExp env e) with
                          | Some TyBool                 -> Some TyBool
                          | _                           -> None)
  | RcvExp name       ->  (match (lookup name env) with
                          | Some TyChan t               -> Some t
                          | _                           -> None)
  | IConst i          ->  Some TyInt
  | BConst b          ->  Some TyBool
  | Var name          ->  (match (lookup name env) with
                          | None                        -> None
                          | aType                       -> aType)
  | FuncExp (name,args) -> (match (lookup name env) with
                          | Some TyFunc (params, ret)        ->  let opTyEq tyOpt ty = (match tyOpt with
                                                                                  | None    -> false
                                                                                  | Some t  -> eqTy t ty) in
                                                            let argTypes = List.map (fun t -> inferTyExp env t) args in
                                                            if (List.for_all (fun (t1, t2) -> opTyEq t1 t2) (List.combine argTypes params))
                                                            then Some ret
                                                            else None
                          | _                            -> None)


(* Implementation of G | (stmt : Cmd | G) where we simply skip Cmd and as above
  use 'option' to report failure

  As discussed, there's a bit of design space when it comes to 'nested' type declarations.
  The below is just a sketch.

*)
let rec typeCheckStmt env stmt = match stmt with
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
                    | _            -> None) (* We only support integer channels *)
  | RcvStmt name  -> (match (lookup name env) with
                    | Some (TyChan TyInt)  -> Some env
                    | _             -> None) (* We only support integer channels *)
  | Decl (n, e)   -> (match (inferTyExp env e) with
                    | Some t        -> Some (update env (n, t)) (* If the expr typechecks, add the binding to the env*)
                    | None          -> None)
  | DeclChan name -> Some (update env (name, TyChan TyInt)) (* We only support integer channels *)
  | Assign (v,e)  -> (match (lookup v env) with
                    | None -> None (* Unknown variable *)
                    | Some t1 -> let t2 = inferTyExp env e in
                                 (match t2 with
                                 | None -> None
                                 | Some t3 -> if eqTy t1 t3
                                              then Some env
                                              else None))
  | While (exp, s) -> (match (inferTyExp env exp) with
                    | Some TyBool -> (match (typeCheckStmt env s) with
                                    | Some _ -> Some env (* Disregard whatever happens inside the block *)
                                    | None   -> None)
                    | _           -> None) (* The condition must be boolean *)
  | ITE (exp, s1, s2) -> (match (inferTyExp env exp) with
                    | Some TyBool -> (match (typeCheckStmt env s1, typeCheckStmt env s2) with
                                    | (Some _, Some _) -> Some env (* Disregard whatever happens inside the blocks *)
                                    | _                -> None)
                    | _           -> None) (* The condition must be boolean *)
  | Return exp     -> (match (inferTyExp env exp) with
                    | Some _ -> Some env
                    | None   -> None)
  | FuncCall (n, args) -> (match (lookup n env) with
                    | Some TyFunc (params, _) ->  let opTyEq tyOpt ty = (match tyOpt with
                                                                      | None    -> false
                                                                      | Some t  -> eqTy t ty) in
                                                      let argTypes = List.map (fun t -> inferTyExp env t) args in
                                                      if (List.for_all (fun (t1, t2) -> opTyEq t1 t2) (List.combine argTypes params))
                                                      then Some env
                                                      else None
                    | _                       -> None)
  | Print exp      -> match (inferTyExp env exp) with
                    | Some _ -> Some env
                    | None   -> None

(* let signatureFromProc p = match p with
  | (name, [], None)        -> ()
  | (name, params, None)    -> expr2
  | (name, [], retType)     -> expr2
  | (name, params, retType) -> expr2 *)


(*

What's still missing are implementations for

(1) collection of type signatures from functions (procedures)

(2) type checking of procedure bodies, and

(3) type checking of the main program.

 *)
