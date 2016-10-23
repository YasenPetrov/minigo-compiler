open Ast;;
open Typecheck;;

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
                let normalProg = normalizeProg result in
                match typeCheckProg normalProg with
                | Some e -> print_string (print_env e); print_string (print_prog result); print_newline(); flush stdout;
                | None -> print_string "Typecheck error";  print_newline(); flush stdout;
            with Parsing.Parse_error ->
                print_string "Parsing error"; print_newline(); flush stdout
          done
        with Lexer.Eof ->
            exit 0
