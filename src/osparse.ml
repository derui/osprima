open Core.Std
open Js_lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Js_parser.parser_main Js_lexer.token lexbuf with
  | Js_parser.Error -> fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some prog ->
     Js_typetree.program_to_json prog |> Tiny_json.Json.format Format.std_formatter
  | None -> ()

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = filename};
  parse_and_print lexbuf;
  In_channel.close inx

let command = Command.basic
  ~summary:"Parse ECMAScript to AST"
  ~readme:(fun () -> "More detailed information")
  Command.Spec.(empty +> anon ("filename" %: string))
  loop

let () =
  Command.run ~version:"0.0.1" ~build_info:"derui" command
