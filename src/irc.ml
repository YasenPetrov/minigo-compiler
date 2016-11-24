type irc = IRC of (irc_cmd list)

and irc_cmd = IRC_Assign of string * irc_exp
            | IRC_Label of int
            | IRC_Goto of int
            | IRC_NonzeroJump of string * int  (* if x L = if x non-zero then jump to L *)
            | IRC_Param of string
            | IRC_Call of int * int (* (label, number of parameters *)
            | IRC_Return of string
            | IRC_Get of string
            (* Added by me *)
            | IRC_Return_void
            | IRC_Push_Ret of int
            | IRC_Transmit of string * string
            | IRC_Rcv of string
            | IRC_Print of string

and irc_exp = IRC_And of string * string
            | IRC_Eq of string * string
            | IRC_Gt of string * string
            | IRC_Plus of string * string
            | IRC_Minus of string * string
            | IRC_Times of string * string
            | IRC_Division of string * string
            | IRC_Not of string
            | IRC_IConst of int
            | IRC_Var of string

and irc_proc = IRC_Proc of int * (string list) * bool * (string list) * (irc_cmd list)(* label * args * returns_value * locals * code *)


let print_irc_exp exp = (match exp with
  | IRC_And (n1, n2) -> String.concat "" ["IRC_And ("; n1; ", "; n2; ")"]
  | IRC_Eq (n1, n2) -> String.concat "" ["IRC_Eq ("; n1; ", "; n2; ")"]
  | IRC_Gt (n1, n2) -> String.concat "" ["IRC_Gt ("; n1; ", "; n2; ")"]
  | IRC_Plus (n1, n2) -> String.concat "" ["IRC_Plus ("; n1; ", "; n2; ")"]
  | IRC_Minus (n1, n2) -> String.concat "" ["IRC_Minus ("; n1; ", "; n2; ")"]
  | IRC_Times (n1, n2) -> String.concat "" ["IRC_Times ("; n1; ", "; n2; ")"]
  | IRC_Division (n1, n2) -> String.concat "" ["IRC_Division ("; n1; ", "; n2; ")"]
  | IRC_Not n -> String.concat "" ["IRC_Not ("; n; ")"]
  | IRC_IConst x -> String.concat "" ["IRC_IConst ("; string_of_int x; ")"]
  | IRC_Var n -> String.concat "" ["IRC_Var ("; n; ")"]
  )

let print_irc_cmd cmd = (match cmd with
  | IRC_Assign (n, e) -> String.concat "" ["IRC_Assign ("; n; ", "; print_irc_exp e; ")"]
  | IRC_Label l -> String.concat "" ["IRC_Label ("; string_of_int l; ")"]
  | IRC_Goto l -> String.concat "" ["IRC_Goto ("; string_of_int l; ")"]
  | IRC_NonzeroJump (n, l) -> String.concat "" ["IRC_NonzeroJump ("; n; ", "; string_of_int l; ")"]
  | IRC_Param n -> String.concat "" ["IRC_Param ("; n ; ")"]
  | IRC_Call (l, p) -> String.concat "" ["IRC_Call ("; string_of_int l; ", "; string_of_int p; ")"]
  | IRC_Return n -> String.concat "" ["IRC_Return ("; n ; ")"]
  | IRC_Get n -> String.concat "" ["IRC_Get ("; n ; ")"]
  (* Added by me *)
  | IRC_Return_void -> "IRC_Return_void"
  | IRC_Push_Ret l -> String.concat "" ["IRC_Push_Ret ("; string_of_int l; ")"]
  | IRC_Transmit (n, l) -> String.concat "" ["IRC_Transmit ("; n; ", ";  l; ")"]
  | IRC_Rcv n -> String.concat "" ["IRC_Rcv ("; n ; ")"]
  | IRC_Print n -> String.concat "" ["IRC_Print ("; n ; ")"]
  )

let print_irc_proc ip = (match ip with
  | IRC_Proc (l, args, ret, locs, code) -> let arg_str = String.concat ", " args in
                                           let loc_str = String.concat ", " locs in
                                           let ret_str = string_of_bool ret in
                                           let code_str = String.concat "\n" (List.map print_irc_cmd code) in
                                           let l_str = string_of_int l in
                                           String.concat "" ["IRC_Proc ("; l_str; ", ("; arg_str; "), "; ret_str; ", ("; loc_str; ") \n"; code_str; "\n)\n"]
  )
