open Irc;;
open Irc_translate;;
open Vm;;
open Exceptions;;
open Typecheck;;

let rec ind_of_helper el lst pos = (match lst with
                    | [] -> None
                    | x::xs -> if x = el then Some pos else ind_of_helper el xs (pos + 1)
                  )

let ind_of elem lst = ind_of_helper elem lst 0


let get_pos_in_env_stack n locals =  let pos = (match (ind_of n locals) with
                                               | None -> raise_var_not_in_act_rec n
                                               | Some x -> x
                                               ) in
                                     let tot_len = List.length locals in
                                     tot_len - pos


let translate_irc_exp exp locals = (match exp with
  | IRC_And (n1, n2) -> let p1, p2 = (get_pos_in_env_stack n1 locals, get_pos_in_env_stack n2 locals) in
                        [
                            AssignFromEnv(p1, 0);
                            AssignFromEnv(p2, 1);
                            PushS 2;
                            PushToStack 0;
                            PushToStack 1;
                            Add; (* n1 + n2 = 2 iff n1 && n2*)
                            Eq
                        ]
  | IRC_Eq (n1, n2) -> let p1, p2 = (get_pos_in_env_stack n1 locals, get_pos_in_env_stack n2 locals) in
                       [
                           AssignFromEnv(p1, 0);
                           AssignFromEnv(p2, 1);
                           PushToStack 0;
                           PushToStack 1;
                           Eq
                       ]
  | IRC_Gt (n1, n2) -> let p1, p2 = (get_pos_in_env_stack n1 locals, get_pos_in_env_stack n2 locals) in
                      [
                          AssignFromEnv(p1, 0);
                          AssignFromEnv(p2, 1);
                          PushToStack 0;
                          PushToStack 1;
                          Gt
                      ]
  | IRC_Plus (n1, n2) -> let p1, p2 = (get_pos_in_env_stack n1 locals, get_pos_in_env_stack n2 locals) in
                       [
                           AssignFromEnv(p1, 0);
                           AssignFromEnv(p2, 1);
                           PushToStack 0;
                           PushToStack 1;
                           Add
                       ]
  | IRC_Minus (n1, n2) -> let p1, p2 = (get_pos_in_env_stack n1 locals, get_pos_in_env_stack n2 locals) in
                       [
                           AssignFromEnv(p1, 0);
                           AssignFromEnv(p2, 1);
                           PushToStack 0;
                           PushToStack 1;
                           Sub
                       ]
  | IRC_Times (n1, n2) -> let p1, p2 = (get_pos_in_env_stack n1 locals, get_pos_in_env_stack n2 locals) in
                       [
                           AssignFromEnv(p1, 0);
                           AssignFromEnv(p2, 1);
                           PushToStack 0;
                           PushToStack 1;
                           Mult
                       ]
  | IRC_Division (n1, n2) -> let p1, p2 = (get_pos_in_env_stack n1 locals, get_pos_in_env_stack n2 locals) in
                       [
                           AssignFromEnv(p1, 0);
                           AssignFromEnv(p2, 1);
                           PushToStack 0;
                           PushToStack 1;
                           Div
                       ]
  | IRC_Not n -> let p = get_pos_in_env_stack n locals in
                       [
                           AssignFromEnv(p, 0);
                           PushS 1;
                           PushToStack 0;
                           Sub (* NOT x = 1 - x *)
                       ]
  | IRC_IConst x -> [
                        PushS x;
                    ]
  | IRC_Var n -> let p = get_pos_in_env_stack n locals in
                       [
                           AssignFromEnv(p, 0);
                           PushToStack 0
                       ]
  )

let rec repeat el x = if x = 0
                      then []
                      else el::(repeat el (x-1))

let rec pushArgs par_count i = if par_count - i < 0 then []
                           else [ AssignFromStack (par_count - i + 1, 0); PushToEnv 0] @ (pushArgs par_count (i + 1))

let translate_irc_cmd cmd locals pc = (match cmd with
  | IRC_Assign (n, exp) -> let p = get_pos_in_env_stack n locals in
                          let exp_code = translate_irc_exp exp locals in
                          exp_code
                          @
                          [
                              AssignFromStack(1, 0);
                              PopS;
                              UpdateToEnv (p, 0)
                          ]
  | IRC_Label l         -> [ Label l ]
  | IRC_Goto  l         -> [ Jump l ]
  | IRC_NonzeroJump (n, l) -> let p = get_pos_in_env_stack n locals in
                           [
                              AssignFromEnv (p, 0);
                              PushToStack 0;
                              NonZero l
                           ]
  | IRC_Push_Ret argcount -> [
                                PushE (pc + 2 + argcount * 5) (* 5 lines per argument, this line and the jump *)
                              ]
  | IRC_Param n          -> let p = get_pos_in_env_stack n locals in
                            [
                                AssignFromEnv (p + 1, 0); (* incr pos by 1 because we've already pushed the RA *)
                                PushToStack 0
                            ]
  | IRC_Call (l, param_count) -> let pushParamsCode = pushArgs param_count 1 in
                                 pushParamsCode @ (repeat (PopS) param_count)
                                 @
                                 [
                                  Jump l
                                 ]
  | IRC_Return n         -> let p = get_pos_in_env_stack n locals in
                            let pop_args_code = repeat (PopE) (List.length locals) in
                            [
                                AssignFromEnv (p, 0)
                            ]
                            @
                            pop_args_code
                            @
                            [
                                AssignFromEnv (1, 1); (* the return address is on top of the stack *)
                                PopE; (* pop RA *)
                                PushToEnv 0; (* push return value *)
                                JumpMemLoc 1
                            ]
  | IRC_Return_void      -> let pop_args_code = repeat (PopE) (List.length locals) in
                            pop_args_code
                            @
                            [
                                AssignFromEnv (1, 1); (* the return address is on top of the stack *)
                                PopE; (* pop RA *)
                                JumpMemLoc 1
                            ]
  | IRC_Get n            -> let p = get_pos_in_env_stack n locals in
                            [
                                AssignFromEnv (1, 0); (* get ret vl, put it in memLoc 0 *)
                                PopE; (* Pop the return value from the RTE stack *)
                                UpdateToEnv (p, 0) (* update var with ret value *)
                            ]
  | IRC_Transmit _       -> [ Skip ] (* Currently not supported *)
  | IRC_Rcv _            -> [ Skip ] (* Currently not supported *)
  | IRC_Print n          -> let p = get_pos_in_env_stack n locals in
                            [
                                AssignFromEnv (p, 0);
                                PushToStack 0;
                                Output;
                                PopS
                            ]
  )


let rec translate_cmd_list cmds locals pc = (match cmds with
  | [] -> []
  | cmd::rest -> let transl_cmd = translate_irc_cmd cmd locals pc in
                 let new_pc = pc + (List.length transl_cmd) in
                 let transl_rest = translate_cmd_list rest locals new_pc in
                 transl_cmd @ transl_rest
  )

let translate_irc_proc proc pc = (match proc with
  | IRC_Proc (l, args, has_ret, locals, body) ->
    let activation_record = args @ locals in
    let init_locals_code = repeat (PushE 0) (List.length locals) in
    let body_code = translate_cmd_list body activation_record (pc + (List.length init_locals_code) + 1) in
    [ Label l]
    @
    init_locals_code
    @
    body_code
  )

let rec translate_irc_procs procs pc = (match procs with
  | []    -> []
  | p::ps -> let p_code = translate_irc_proc p pc in
             let new_pc = pc + (List.length p_code) in
             p_code
             @
             translate_irc_procs ps new_pc
  )

let rec collect_label_locations cmds pc map = (match cmds with
  | [] -> map
  | (Label l)::rest -> collect_label_locations rest (pc + 1) (map @ [(l, pc)])
  | c::rest         -> collect_label_locations rest (pc + 1) map
  )

let rec replace_labels instrs = (match instrs with
  | [] -> []
  | (Label _)::rest -> Skip::(replace_labels rest)
  | c::rest -> c::(replace_labels rest)
)

let symb_to_pc cmd lbl_to_pc_map = (match cmd with
  | Jump l -> (match lookup l lbl_to_pc_map with
      | Some pc -> Jump pc
      | None -> raise_label_not_in_map l;)
  | NonZero l -> (match lookup l lbl_to_pc_map with
      | Some pc -> NonZero pc
      | None -> raise_label_not_in_map l;)
  | Zero l -> (match lookup l lbl_to_pc_map with
      | Some pc -> Zero pc
      | None -> raise_label_not_in_map l;)
  | c -> c
  )

let translate_irc_prog procs main main_ar = let translated_procs = translate_irc_procs procs 1 in
                                            let new_pc = (List.length translated_procs) + 1 in
                                            let init_main_locs_code = repeat (PushE 0) (List.length main_ar) in
                                            let translated_main = init_main_locs_code
                                                                  @
                                                                  (translate_cmd_list main main_ar (new_pc + (List.length main_ar))) in
                                            let full_code = translated_procs @ translated_main in
                                            let lbl_to_pc = collect_label_locations full_code 1 [] in
                                            let full_no_labels = replace_labels full_code in
                                            let final_code = List.map (fun x -> symb_to_pc x lbl_to_pc) full_no_labels in
                                            [ Jump new_pc ] @ final_code @ [ Halt ]
