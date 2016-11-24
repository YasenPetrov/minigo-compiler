open Ast;;
open Typecheck;;
open Ast_normalize;;
open Irc;;
open Irc_translate;;
open Vm;;
open Vm_translate;;
open Exceptions;;

let read_file filename =
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; !lines
with End_of_file ->
  close_in chan;
  List.rev !lines ;;

let oc = open_out "l.txt" in
Printf.fprintf oc "%s\n" (String.concat " " (read_file "test.txt"));
close_out oc;

let ch = open_in "l.txt" in
        try
          let lexbuf = Lexing.from_channel ch in
          while true do
            try
              let result = Parser.main Lexer.token lexbuf in
              let raw_str = print_prog result in

              let normalProg, lookup, dict = rename_variables_prog result in
              let normal_str = print_prog normalProg in

              let (typechecked_prog, main_env), fn_names = typeCheckProg normalProg in
              let typechecked_str = print_prog typechecked_prog in

              let (irc_procs, irc_main, main_ar) = translateProg typechecked_prog fn_names in
              let irc_p_str = String.concat "\n" (List.map print_irc_proc irc_procs) in
              let irc_m_str = String.concat "\n" (List.map print_irc_cmd irc_main) in
              let main_ar_str = String.concat "\n" main_ar in

              let vm_code = translate_irc_prog irc_procs irc_main main_ar in
              let vm_code_str = print_instr_list vm_code in
                match main_env with
                  | Some e -> print_string "Parsed: \n\n";
                              print_string raw_str; print_string "\n\n ============ \n\n";
                              print_string "Normalized:\n\n";
                              print_string normal_str; print_string "\n\n ============ \n\n";
                              print_string "Typechecked:\n\n";
                              print_string typechecked_str; print_string "\n\n ============ \n\n";
                              print_string "Environment: \n\n";
                              print_string (print_env e); print_string "\n\n ============ \n\n";
                              print_string "Main activation record: \n\n";
                              print_string main_ar_str; print_string "\n\n ============ \n\n";
                              print_string "Intermediate code (procs): \n\n";
                              print_string irc_p_str; print_string "\n\n ------------ \n\n";
                              print_string "Intermediate code (main): \n\n";
                              print_string irc_m_str; print_string "\n\n ============ \n\n";
                              print_string "VM code: \n\n";
                              print_string vm_code_str; print_string "\n\n ============ \n\n";
                              flush stdout;
                              run vm_code
                  | None -> print_string "Typecheck error";
                            print_newline();
                            flush stdout;
                            run vm_code
            with Parsing.Parse_error ->
                raise(ParsingError)
          done
        with Lexer.Eof ->
            exit 0
