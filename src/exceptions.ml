open Ast;;

exception TypeError of string
exception NoSuchKeyError of string
exception ParsingError
exception UnexpectedError of string

let raise_undecl_var n = raise (TypeError (String.concat "" ["Undeclared variable: "; n]))

let raise_expected_other_type exp act = raise (TypeError (String.concat "" ["Type mismatch: expected "; print_type exp; " but got "; print_type_opt act]))

let raise_cannot_compare t1 t2 = raise (TypeError (String.concat "" ["Type mismatch: cannot compare "; print_type t1; " and "; print_type t2]))

let raise_function_arguments_mismatch name given actual = let given_string = String.concat ", " (List.map print_type given) in
                                                     let act_string = String.concat ", " (List.map print_type_opt actual) in
                                                     raise (TypeError (String.concat ""
                                                     ["Function arguments mismatch: "; name; " expected ("; act_string; ") but got ("; given_string; ")"]))

let raise_function_count_mismatch name actual given = raise (TypeError (
  String.concat "" ["Wrong number of function arguments: "; name; " expected ";
  string_of_int actual; " arguments but got "; string_of_int given]))

let raise_not_a_function name = raise (TypeError (String.concat "" [name; " is not a function!"]))

let raise_function_redeclaration name = raise (TypeError (String.concat "" [name; " is already a name of a function. You can't assign something else to it."]))

let raise_return_in_void_function n = raise (TypeError (String.concat ""
    ["The function "; n; " was declared void but returns a value"]))

let raise_returns_wrong_type n given actual = raise (TypeError (String.concat ""
    ["The function "; n; "was declared to return "; print_type actual; " but returns a value of type "; print_type given]))

let raise_cannot_be_function_param exp = raise (TypeError (String.concat ""
    [print_exp exp; " cannot be a function parameter"]))

let raise_no_top_level_return_statement n = raise (TypeError (String.concat ""
    ["In function "; n; ": no top level return statement"]))

let raise_cannot_declate_var_and_assign_to_chan var_n = raise (TypeError (String.concat ""
    ["You are trying to declare "; var_n; " with the := operator and set its value to that of a channel. Please don't. Thank you. Very much. You're a sweetheart."]))

let raise_no_such_key key = raise (NoSuchKeyError (String.concat ""
    ["The key "; key; " is not present in the dictionary - maybe this variable has not been declared?"]))

let raise_no_such_function n = raise(UnexpectedError (String.concat ""
    [n; " has not been recorded as a function - I'm doing something wrong"]))

let raise_symb_label_in_vm_code l = raise(UnexpectedError (String.concat ""
    ["I have left symbolic label l"; string_of_int l; " in the VM code - this is not ideal, I'm doing something wrong"]))

let raise_var_not_in_act_rec n = raise(UnexpectedError (String.concat ""
    [n; " cannot be found in the current activation record - whoever made this compiler should not be allowed near a keyboard."]))

let raise_label_not_in_map l = raise(UnexpectedError (String.concat ""
    ["The label l"; string_of_int l; " is not in the symb_label -> pc map. A 1000 monkeys with a 1000 typewriters would write a better compiler."]))
