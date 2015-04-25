{
  open CamomileLibrary
  module Def = CamomileLibraryDefault.Camomile
  open Js_parser
  module UReStr = Def.UReStr
  module R = UReStr.Make(Def.UTF8)

  let next_line lexbuf = Lexing.new_line lexbuf

  let has_line_terminator text =
    let regexp = UReStr.regexp ".*\\(\r\\|\n\\|\r\n\\|\x20\x28\\|\x20\x29\\)" in
    let regexp = R.compile regexp in
    R.string_match regexp text 0

  let line_terminators_in_text text =
    let regexp = UReStr.regexp "\\(\r\\|\n\\|\r\n\\|\x20\x28\\|\x20\x29\\)" in
    let regexp = R.compile regexp in
    match R.regexp_match regexp text 0 with
    | None -> [||]
    | Some ary -> ary

  let to_keyword = function
    |  "this"  ->  KEYWORD_THIS 
    |  "get"  ->  KEYWORD_GET 
    |  "set"  ->  KEYWORD_SET 
    |  "new"  ->  KEYWORD_NEW 
    |  "in"  ->  KEYWORD_IN 
    |  "instanceof"  ->  KEYWORD_INSTANCEOF 
    |  "delete"  ->  KEYWORD_DELETE 
    |  "typeof"  ->  KEYWORD_TYPEOF 
    |  "function"  ->  KEYWORD_FUNCTION 
    |  "void"  ->  KEYWORD_VOID 
    |  "var"  ->  KEYWORD_VAR 
    |  "if"  ->  KEYWORD_IF 
    |  "else"  ->  KEYWORD_ELSE 
    |  "do"  ->  KEYWORD_DO 
    |  "while"  ->  KEYWORD_WHILE 
    |  "for"  ->  KEYWORD_FOR 
    |  "continue"  ->  KEYWORD_CONTINUE 
    |  "break"  ->  KEYWORD_BREAK 
    |  "return"  ->  KEYWORD_RETURN 
    |  "with"  ->  KEYWORD_WITH 
    |  "switch"  ->  KEYWORD_SWITCH 
    |  "case"  ->  KEYWORD_CASE 
    |  "default"  ->  KEYWORD_DEFAULT 
    |  "throw"  ->  KEYWORD_THROW 
    |  "try"  ->  KEYWORD_TRY 
    |  "catch"  ->  KEYWORD_CATCH 
    |  "finally"  ->  KEYWORD_FINALLY 
    |  "debugger"  ->  KEYWORD_DEBUGGER
    |  "null"  ->  NULL
    | "true" -> TRUE
    | "false" -> FALSE
    | s -> failwith ("Unknown keyword: " ^ s)
}
let line_terminator = ['\n' '\r'] | "\r\n" | "\x20\x28" | "\x20\x29"
let white_space = [' ' '\t' '\x0b' '\x0c' '\xa0']
let identifier_start = ['a'-'z' 'A'-'Z'] | '$' | '_'
let decimal_integer_literal = '0' | (['1'-'9'] ['0'-'9']*) 
let exponent_part = ['e' 'E'] ['-' '+']? ['0'-'9']+
  let reserved_word = "true" | "false" | "null" | "this" | "get" | "set" | "new"
    | "in" | "instanceof" | "delete" | "typeof" | "function" | "void"
    | "var" | "if" | "else" | "do" | "while"
    | "for" | "continue" | "break" | "return" | "with"
    | "switch" | "case" | "default" | "throw" | "try"
    | "catch" | "finally" | "debugger" | "null" | "true" | "false"

        (* Json Tokens *)
        rule token = parse
    | "/*" { multi_line_comment "" lexbuf}
    | "//" { single_line_comment "" lexbuf}
    | '$' { DOLLAR }
    | '_' { UNDERSCORE }
    | '{' { LCBRACE }
    | '(' { LPAREN }
    | '[' { LBRACE }
    | '}' { RCBRACE }
    | ')' { RPAREN }
    | ']' { RBRACE }
    | ':' { COLON }
    | ';' { SEMICOLON }
    | ',' { COMMA }
    | '-' { MINUS }
    | '+' { PLUS }
    | '<' { LESS }
    | '>' {GREATER}
    | "<=" {LESS_THAN}
    | ">=" {GREATER_THAN}
    | "==" { EQUAL}
    | "!=" {NOT_EQUAL}
    | "===" {DEEP_EQUAL}
    | "!==" {DEEP_NOT_EQUAL}
    | '*' {MULTI}
    | '%' {MOD}
    | "++" {INCREMENT}
    | "--" {DECREMENT}
    | "<<" {LSHIFT}
    | ">>" {RSHIFT}
    | '&' {AND}
    | '|' {OR}
    | '^' {XOR}
    | '!' {NOT}
    | '~' {COMP}
    | "&&" {LOGICAL_AND}
    | "||" {LOGICAL_OR}
    | '?' {QUESTION}
    | '=' {ASSIGN}
    | "+=" {PLUS_ASSIGN}
    | "-=" {MINUS_ASSIGN}
    | "*=" {MULTI_ASSIGN}
    | "%=" {MOD_ASSIGN}
    | "<<=" {LSHIFT_ASSIGN}
    | ">>=" {RSHIFT_ASSIGN}
    | "&=" {AND_ASSIGN}
    | "|=" {OR_ASSIGN}
    | "^=" {XOR_ASSIGN}
    | '/' {DIV}
    | "/=" {DIV_ASSIGN}
    | decimal_integer_literal exponent_part? as digit { DECIMAL_LITERAL(digit)}
    | decimal_integer_literal '.' ['0'-'9']* exponent_part? as digit { DECIMAL_LITERAL(digit)}
    | '.' ['0'-'9']+ exponent_part? as digit { DECIMAL_LITERAL(digit)}
    | '0' ['x' 'X'] ['a'-'f' 'A'-'F' '0'-'9']+ as digit { HEX_DIGIT(digit) }
    | '.' { DOT }
    | '"' { string_parse "" lexbuf}
    | '\'' { single_string_parse "" lexbuf }
    | eof  {EOF}
    | [' ' '\t' '\x0b' '\x0c' '\xa0'] { token lexbuf}
    | line_terminator {next_line lexbuf; token lexbuf}
    | reserved_word { to_keyword (Lexing.lexeme lexbuf)}
    | identifier_start (identifier_start | ['0'-'9'])* as ident {IDENT(ident)}

  and single_line_comment buf = parse
      | _ {
        let buf = buf ^ (Lexing.lexeme lexbuf) in
        if has_line_terminator buf then begin
          next_line lexbuf;
          SINGLE_LINE_COMMENT(buf)
        end
        else single_line_comment buf lexbuf
      }
  and multi_line_comment buf = parse
      | [^ '*'] _* "*/" as comment {
        let terminators = line_terminators_in_text comment in
        Array.iter (fun _ -> next_line lexbuf) terminators;
        MULTI_LINE_COMMENT(comment)}
      | '*' [^ '*' '/']* "*/" as comment {
        let terminators = line_terminators_in_text comment in
        Array.iter (fun _ -> next_line lexbuf) terminators;
        MULTI_LINE_COMMENT(comment)
      }
  and string_parse buf = parse
    [^ '"']* '"' as str {STRING(str)}
  and single_string_parse buf = parse
    [^ '\'']* '\'' as str {STRING(str)}
