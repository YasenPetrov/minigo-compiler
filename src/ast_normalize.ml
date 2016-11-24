open Ast;;
open Exceptions;;

type nameMap = Dict of (string * int) list

let rec print_namegen dict = (match dict with
  | Dict ls -> (match ls with
    | [] -> ""
    | (n, x)::rest -> let rest_str = print_namegen (Dict rest) in
                         String.concat "" [n; " -> "; string_of_int x; "\n"; rest_str]
    )
  )

let rec updateLookup name value lookup = (match lookup with
  | Dict ls -> (match ls with
    | [] -> Dict([(name, value)])
    | (n, x)::rest -> if name = n
                        then Dict((n, value)::rest)
                        else match (updateLookup name value (Dict rest)) with
                          | Dict ls -> Dict((n, x)::ls)
    )
  )

let rec addToDict name dict = (match dict with
  | Dict ls -> (match ls with
    | [] -> Dict [(name, 1)]
    | (n,v)::tps -> if n = name
                       then dict
                       else (match (addToDict name (Dict tps)) with
                            | Dict lst -> Dict ((n,v)::tps))
    )
  )

let rec generateNewName name dict = (match dict with
  | Dict ls -> (match ls with
    | [] -> (String.concat "" [name; "_1_"], Dict [(name, 1)])
    | tp::tps -> (match tp with
                  |(n, x) ->  if name = n
                              then (String.concat "" [n; "_"; string_of_int (x + 1); "_"],
                                    Dict ((n, x + 1)::tps))
                              else (match (generateNewName name (Dict tps)) with
                                | str, (Dict lst) -> (str, Dict (tp::lst))
                                )
                  )
    )
  )

let rec lookupName name dict = (match dict with
  | Dict ls -> (match ls with
    | [] -> raise_no_such_key name
    | (n, x)::rest -> if name = n
                        then String.concat "" [n; "_"; string_of_int x; "_"]
                        else lookupName name (Dict rest)
    )
  )

let rec lookupNum name dict = (match dict with
  | Dict ls -> (match ls with
    | [] -> raise_no_such_key name
    | (n, x)::rest -> if name = n
                        then x
                        else lookupNum name (Dict rest)
    )
  )

let rec rename_vars_expr expr dict = (match expr with
  | And (e1, e2) -> And(rename_vars_expr e1 dict, rename_vars_expr e2 dict)
  | Eq (e1, e2) -> Eq(rename_vars_expr e1 dict, rename_vars_expr e2 dict)
  | Gt (e1, e2) -> Gt(rename_vars_expr e1 dict, rename_vars_expr e2 dict)
  | Plus (e1, e2) -> Plus(rename_vars_expr e1 dict, rename_vars_expr e2 dict)
  | Minus (e1, e2) -> Minus(rename_vars_expr e1 dict, rename_vars_expr e2 dict)
  | Times (e1, e2) -> Times(rename_vars_expr e1 dict, rename_vars_expr e2 dict)
  | Division (e1, e2) -> Division(rename_vars_expr e1 dict, rename_vars_expr e2 dict)
  | Not e -> Not (rename_vars_expr e dict)
  | RcvExp n -> RcvExp(lookupName n dict)
  | IConst x -> IConst x
  | BConst x -> BConst x
  | Var n -> Var(lookupName n dict)
  | FuncExp (n, exs) -> let new_exs = List.map (fun e -> rename_vars_expr e dict) exs in
                        FuncExp(n, new_exs)

)
(* :: (stmt, nameGen) -> (stmt, nameGen) *)
let rec rename_vars_stmt stmt lookup dict = (match stmt with
  | Seq (s1, s2)  -> let (st1, new_lu, new_dict) = rename_vars_stmt s1 lookup dict in
                     let (st2, newer_lu, newer_dict) = rename_vars_stmt s2 new_lu new_dict in
                     (Seq(st1, st2), newer_lu, newer_dict)
  | Go s          -> let (st, _, new_dict) = rename_vars_stmt s lookup dict in
                     (Go st, lookup, new_dict)
  | Transmit (n, e) -> let new_name = lookupName n lookup in
                       let new_expr = rename_vars_expr e lookup in
                       (Transmit(new_name, new_expr), lookup, dict)
  | RcvStmt name  -> let new_name = lookupName name lookup in
                     (RcvStmt new_name, lookup, dict)
  | Decl (n, e)   -> let new_name, new_dict = generateNewName n dict in
                     let new_lookup = updateLookup n (lookupNum n new_dict) lookup in
                     let new_expr = rename_vars_expr e lookup in(* use the old lookup to rename here *)
                     (Decl(new_name, new_expr), new_lookup, new_dict)
  | DeclChan n    -> let new_name, new_dict = generateNewName n dict in
                     let new_lookup = updateLookup n (lookupNum n new_dict) lookup in
                     (DeclChan new_name, new_lookup, new_dict)
  | Assign (n, e) -> let new_name = lookupName n lookup in
                     let new_expr = rename_vars_expr e lookup in
                     (Assign(new_name, new_expr), lookup, dict)
  | While (e, (l, s)) -> let new_expr = rename_vars_expr e lookup in
                         let new_st, _, new_dict = rename_vars_stmt s lookup dict in
                         (While (new_expr, (l, new_st)), lookup, new_dict)
  | ITE (e, (l1, s1), (l2, s2)) -> let new_expr = rename_vars_expr e lookup in
                         let new_st1, _, new_dict = rename_vars_stmt s1 lookup dict in
                         let new_st2, _, newer_dict = rename_vars_stmt s2 lookup new_dict in
                         (ITE (new_expr, (l1, new_st1), (l2, new_st2)), lookup, newer_dict)
  | Return e     -> let new_expr = rename_vars_expr e lookup in
                    (Return new_expr, lookup, dict)
  | FuncCall (n, exs) -> let new_exs = List.map (fun e -> rename_vars_expr e lookup) exs in
                        (FuncCall(n, new_exs), lookup, dict)
  | Print e      -> let new_expr = rename_vars_expr e lookup in
                    (Print new_expr, lookup, dict)
  | Skip         -> Skip, lookup, dict
)


let rec rename_params params dict = (match params with
  | [] -> [], dict
  | (Var n, t)::rest -> let new_name, new_dict = generateNewName n dict in
                        let new_rest, newer_dict = rename_params rest new_dict in
                        (Var new_name, t)::new_rest, newer_dict
  )


let rec rename_variables_procs procs dict = (match procs with
  | [] -> ([], dict)
  | proc::rest -> (match proc with
    | Proc (n, params, tps, (locs, stmt)) -> let new_params, new_dict = rename_params params dict in
                                             let new_st, _, newer_dict = rename_vars_stmt stmt new_dict new_dict in
                                             let new_proc = Proc(n, new_params, tps, (locs, new_st)) in
                                             let new_rest, newest_dict = rename_variables_procs rest newer_dict in
                                             (new_proc::new_rest, newest_dict)
  ))


let rename_variables_prog prog = (match prog with
  | Prog (procs, block) -> let new_procs, new_dict = rename_variables_procs procs (Dict []) in
                           let new_block, final_lookup, final_dict = rename_vars_stmt block new_dict new_dict in
                           Prog(new_procs, new_block), final_lookup, final_dict
  )
