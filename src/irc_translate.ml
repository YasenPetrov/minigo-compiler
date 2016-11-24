open Ast;;
open Irc;;
open Typecheck;;
open Exceptions;;

let labelSupply = ref 1
let freshLabel _ =  labelSupply := !labelSupply + 1;
                    !labelSupply

let nameLblSupply = ref 1
let freshName _ = nameLblSupply := !nameLblSupply + 1;
                  String.concat "" ["__t"; (string_of_int !nameLblSupply)]



(* short-hand for 'zero' jump *)
let irc_ZeroJump (x,l) = let y = freshName() in
                         [IRC_Assign (y, IRC_Not x);
                          IRC_NonzeroJump (y,l)]

(* (parts) of translation of Booleans (short-circuit evaluation!),
   yields a tuple where first component represents the IRC and
   second component a variable name which is bound to the result *)
let rec translateB exp funcToLabel = (match exp with
  | BConst true -> let x = freshName() in
                   ([IRC_Assign (x, IRC_IConst 1)], x)
  | BConst false -> let x = freshName() in
                    ([IRC_Assign (x, IRC_IConst 0)], x)
  | IConst i     -> let x = freshName() in
                    ([IRC_Assign (x, IRC_IConst i)], x)
  | And (e1, e2) -> (*
                       e1.code;
                       if !e1.result goto l1
                       e2.code;
                       x = e2.result;
                       goto l2;
                       l1:
                       x = 0;             Booleans represented as integers
                       l2:
                     *)

                    let r1 = translateB e1 funcToLabel in
                    let r2 = translateB e2 funcToLabel in
                    let x = freshName() in
                    let l1 = freshLabel() in
                    let l2 = freshLabel() in
                    ((fst r1)
                     @
                     (irc_ZeroJump (snd r1,l1))
                     @
                     (fst r2)
                     @
                     [IRC_Assign (x, IRC_Var (snd r2));
                      IRC_Goto l2 ]
                     @
                     [IRC_Label l1;
                      IRC_Assign (x, IRC_IConst 0);
                      IRC_Label l2],
                     x)
 | Eq (e1, e2) -> (*
                      e1.code;
                      e2.code;
                      x = e1.result == e2.result
                  *)

                   let r1 = translateB e1 funcToLabel in
                   let r2 = translateB e2 funcToLabel in
                   let x = freshName() in
                   ((fst r1)
                    @
                    (fst r2)
                    @
                    [IRC_Assign (x, IRC_Eq (snd r1, snd r2))],
                    x)
  | Gt (e1, e2) -> (*
                       e1.code;
                       e2.code;
                       x = e1.result > e2.result
                   *)

                    let r1 = translateB e1 funcToLabel in
                    let r2 = translateB e2 funcToLabel in
                    let x = freshName() in
                    ((fst r1)
                     @
                     (fst r2)
                     @
                     [IRC_Assign (x, IRC_Gt (snd r1, snd r2))],
                     x)
 | Plus (e1, e2) -> (*
                        e1.code;
                        e2.code;
                        x = e1.result + e2.result
                      *)

                     let r1 = translateB e1 funcToLabel in
                     let r2 = translateB e2 funcToLabel in
                     let x = freshName() in
                     ((fst r1)
                      @
                      (fst r2)
                      @
                      [IRC_Assign (x, IRC_Plus (snd r1, snd r2))],
                      x)
  | Minus (e1, e2) -> (*
                         e1.code;
                         e2.code;
                         x = e1.result - e2.result
                      *)
                      let r1 = translateB e1 funcToLabel in
                      let r2 = translateB e2 funcToLabel in
                      let x = freshName() in
                      ((fst r1)
                       @
                       (fst r2)
                       @
                       [IRC_Assign (x, IRC_Minus (snd r1, snd r2))],
                       x)
   | Times (e1, e2) -> (*
                          e1.code;
                          e2.code;
                          x = e1.result * e2.result
                        *)
                       let r1 = translateB e1 funcToLabel in
                       let r2 = translateB e2 funcToLabel in
                       let x = freshName() in
                       ((fst r1)
                        @
                        (fst r2)
                        @
                        [IRC_Assign (x, IRC_Times (snd r1, snd r2))],
                        x)
  | Division (e1, e2) -> (*
                     e1.code;
                     e2.code;
                     x = e1.result == e2.result
                   *)
                    let r1 = translateB e1 funcToLabel in
                    let r2 = translateB e2 funcToLabel in
                    let x = freshName() in
                    ((fst r1)
                     @
                     (fst r2)
                     @
                     [IRC_Assign (x, IRC_Division (snd r1, snd r2))],
                     x)
   | Not e           -> (*
                           e.code;
                           x = !e.result
                        *)
                        let r = translateB e funcToLabel in
                        let x = freshName() in
                        ((fst r)
                          @
                          [IRC_Assign (x, IRC_Not (snd r))],
                          x)
  | Var n             -> let x = freshName() in
                          ([IRC_Assign (x, IRC_Var n)], x)
  | FuncExp (n, args)    ->  let transArgs = List.map (fun x -> translateB x funcToLabel) args in
                          let x = freshName() in
                          let func_label = (match (lookup n funcToLabel) with
                                              | None -> raise_no_such_function n
                                              | Some label -> label
                                           ) in
                          (List.concat [
                            (List.concat (List.map (fun (ist, _) -> ist) transArgs));
                            [ IRC_Push_Ret (List.length args) ];
                            (List.map (fun (_, v) -> IRC_Param v) transArgs);
                            [
                              IRC_Call (func_label, (List.length args));
                              IRC_Get x
                            ]
                          ],
                          x)
  | RcvExp n          -> let x = freshName() in
                         ([ IRC_Rcv x], x)
)

  let rec translateStmt stmt funcToLabel = (match stmt with
    | Seq (s1, s2) -> let st1_tr = translateStmt s1 funcToLabel in
                      let st2_tr = translateStmt s2 funcToLabel in
                      st1_tr @ st2_tr
    | Go s         -> translateStmt s funcToLabel
    | Transmit (ch, exp)  -> let (code, var) = translateB exp funcToLabel in
                             code
                             @
                             [
                                IRC_Transmit (ch, var)
                             ]
    | RcvStmt ch          -> [
                                IRC_Rcv ch
                             ]
    | Decl (n, exp)       -> let (code, var) = translateB exp funcToLabel in
                             code
                             @
                             [
                                IRC_Assign (n, (IRC_Var var))
                             ]
    | DeclChan n          -> []
    | Assign (n, exp)     -> let (code, var) = translateB exp funcToLabel in
                             code
                             @
                             [
                                IRC_Assign (n, (IRC_Var var))
                             ]
    | While (cond, (_,s))     -> let (cond_code, cond_var) = translateB cond funcToLabel in
                             let beg = freshLabel() in
                             let en = freshLabel() in
                             [
                                IRC_Label beg;
                             ]
                             @
                             cond_code
                             @
                             irc_ZeroJump (cond_var, en)
                             @
                             translateStmt s funcToLabel
                             @
                             [
                                IRC_Goto beg;
                                IRC_Label en
                             ]
    | ITE (cond, (_, s1), (_, s2))  -> let (cond_code, cond_var) = translateB cond funcToLabel in
                             let l1 = freshLabel() in
                             let l2 = freshLabel() in
                             cond_code
                             @
                             irc_ZeroJump (cond_var, l1)
                             @
                             translateStmt s1 funcToLabel
                             @
                             [
                                IRC_Goto l2;
                                IRC_Label l1
                             ]
                             @
                             translateStmt s2 funcToLabel
                             @
                             [ IRC_Label l2 ]
    | Return exp          -> let code, var = translateB exp funcToLabel in
                             code
                             @
                             [ IRC_Return var ]
    | FuncCall (n, args)  -> let transArgs = List.map (fun x -> translateB x funcToLabel) args in
                            let func_label = (match (lookup n funcToLabel) with
                                                | None -> raise_no_such_function n
                                                | Some label -> label
                                             ) in
                            List.concat [
                              (List.concat (List.map (fun (ist, _) -> ist) transArgs));
                              [ IRC_Push_Ret (List.length args) ];
                              (List.map (fun (_, v) -> IRC_Param v) transArgs);
                              [
                                IRC_Call (func_label, (List.length args))
                              ]
                            ]
    | Print exp           -> let code, var = translateB exp funcToLabel in
                             code
                             @
                             [ IRC_Print var ]
    | Skip                -> []
    )


let rec names_from_args args = (match args with
  | [] -> []
  | (Var x, _)::rest -> x::(names_from_args rest)
  | sth::rest -> names_from_args rest
  )

let names_from_locs locs = (match locs with
  | Locals lst -> List.map (fun x -> fst(x)) lst
  )

let add_to_set el set = if (List.mem el set) then set else set @ [el]

let rec collect_locs_with_temps cmds locs = (match cmds with
  | [] -> locs
  | c::cs -> (match c with
              | IRC_Assign (n, _) -> let new_locs = add_to_set n locs in
                                     collect_locs_with_temps cs new_locs
              | IRC_Get n      -> let new_locs = add_to_set n locs in
                                     collect_locs_with_temps cs new_locs
              | _              -> collect_locs_with_temps cs locs
              )
  )

let translateProc proc funcToLabel = (match proc with
  | Proc (n, args, ret, (locs, body)) -> let has_return = (match ret with
                                                    | TyVoid -> false
                                                    | _ -> true
                                                  ) in
                                 let dummy_return = if has_return then [] else [ IRC_Return_void ] in
                                 let argnames = names_from_args args in
                                 let label = (match (lookup n funcToLabel) with
                                                     | None -> raise_no_such_function n
                                                     | Some label -> label
                                                     ) in
                                 let code = (translateStmt body funcToLabel) @ dummy_return in
                                 let locnames = collect_locs_with_temps code [] in
                                 IRC_Proc (label, argnames, has_return, locnames, code)
  )

let translateProcs procs funcToLabel = List.map (fun p -> translateProc p funcToLabel) procs

let translateProg prog fn_names = (match prog with
  | Prog (procs, main) -> let main_label = freshLabel() in
                          let funcToLabel = List.map (fun n -> (n, freshLabel())) fn_names in
                          let irc_procs = translateProcs procs funcToLabel in
                          let irc_main = [IRC_Label main_label] @ translateStmt main funcToLabel in
                          let main_activation_record = collect_locs_with_temps irc_main [] in
                          irc_procs, irc_main, main_activation_record
  )
