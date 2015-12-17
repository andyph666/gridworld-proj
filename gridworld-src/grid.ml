open Printf
open Analyzer

let _ =
let lexbuf = Lexing.from_channel stdin in
let program = Parser.program Scanner.token lexbuf in
let sast = Analyzer.check_program program in
let pycode = Compile.translate program in
print_endline pycode;;

