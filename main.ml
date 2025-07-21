open Lexer
open Parser
open Interp
open Directive

let () =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    let ast = Parser.program Lexer.token lexbuf in
    close_in ic;

    (* Generate directive list from AST *)
    let directives = Compile.generate_asm ast in

    (* Convert directives to strings *)
    let asm_lines =
      List.map (fun d -> Directive.string_of_directive ~macos:false d) directives
    in

    (* Build output path and open *)
    let output_file = Filename.remove_extension filename ^ ".asm" in
    let output_path = "output/" ^ output_file in
    let oc = open_out output_path in

    (* Write lines to file *)
    List.iter (fun line -> output_string oc (line ^ "\n")) asm_lines;
    close_out oc;

    Printf.printf "Generated assembly: %s\n" output_path

  with
  | Lexer.Error msg ->
      Printf.eprintf "Lexing error: %s\n" msg
  | Parser.Error ->
      Printf.eprintf "Parse error at offset %d\n" (Lexing.lexeme_start lexbuf)
