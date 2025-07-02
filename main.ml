let () =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    let ast = Parser.program Lexer.token lexbuf in
    close_in ic;
    let cpp_code = Codegen.generate ast in
    let output_file = Filename.remove_extension filename ^ ".cpp" in
    let oc = open_out ("output/" ^ output_file) in
    output_string oc cpp_code;
    close_out oc;
    Printf.printf "Generated: output/%s\n" output_file
  with
  | Lexer.Error msg ->
      Printf.eprintf "Lexing error: %s\n" msg
  | Parser.Error ->
      Printf.eprintf "Parse error at offset %d\n" (Lexing.lexeme_start lexbuf)

