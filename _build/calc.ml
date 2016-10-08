open Ast;;

let _ =
          try
            let lexbuf = Lexing.from_channel stdin in
            while true do
              try
                let result = Parser.main Lexer.token lexbuf in
                  print_string (print_prog result); print_newline(); flush stdout
              with Parsing.Parse_error ->
                  print_string "Error"; print_newline(); flush stdout
            done
          with Lexer.Eof ->
            exit 0
