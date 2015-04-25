{
  open Js_parser

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
  | "catch" | "finally" | "debugger" 

      (* Json Tokens *)
      rule token = parse
  | "/*" { multi_line_comment "" lexbuf}
  | "//" {single_line_comment "" lexbuf}
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
  | "true" { TRUE }
  | "false" {FALSE}
  | "null" {NULL}
  | "this" { KEYWORD_THIS }
  | "get" { KEYWORD_GET }
  | "set" { KEYWORD_SET }
  | "new" { KEYWORD_NEW }
  | "in" { KEYWORD_IN }
  | "instanceof" { KEYWORD_INSTANCEOF }
  | "delete" { KEYWORD_DELETE }
  | "typeof" { KEYWORD_TYPEOF }
  | "function" { KEYWORD_FUNCTION }
  | "void" { KEYWORD_VOID }
  | "var" { KEYWORD_VAR }
  | "if" { KEYWORD_IF }
  | "else" { KEYWORD_ELSE }
  | "do" { KEYWORD_DO }
  | "while" { KEYWORD_WHILE }
  | "for" { KEYWORD_FOR }
  | "continue" { KEYWORD_CONTINUE }
  | "break" { KEYWORD_BREAK }
  | "return" { KEYWORD_RETURN }
  | "with" { KEYWORD_WITH }
  | "switch" { KEYWORD_SWITCH }
  | "case" { KEYWORD_CASE }
  | "default" { KEYWORD_DEFAULT }
  | "throw" { KEYWORD_THROW }
  | "try" { KEYWORD_TRY }
  | "catch" { KEYWORD_CATCH }
  | "finally" { KEYWORD_FINALLY }
  | "debugger" { KEYWORD_DEBUGGER }
  | decimal_integer_literal exponent_part? as digit { DECIMAL_LITERAL(digit)}
  | decimal_integer_literal '.' ['0'-'9']* exponent_part? as digit { DECIMAL_LITERAL(digit)}
  | '.' ['0'-'9']+ exponent_part? as digit { DECIMAL_LITERAL(digit)}
  | '0' ['x' 'X'] ['a'-'f' 'A'-'F' '0'-'9']+ as digit { HEX_DIGIT(digit) }
  | '.' { DOT }
  | '"' { DOUBLE_QUOTE }
  | '\'' { SINGLE_QUOTE }
  | ['e' 'E'] as exp { EXP(exp) }
  | '\\' ['"' '\\' '/' 'b' 'f' 'n' 'r' 't']  as ctl { CONTROL_CHAR(ctl) }
  | '\\' ['\'' '\\' '/' 'b' 'f' 'n' 'r' 't']  as ctl { SINGLE_CONTROL_CHAR(ctl) }
  | "\\u"['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F'] as ctl { CONTROL_CHAR(ctl) }
  | eof  {EOF}
  | [' ' '\t' '\x0b' '\x0c' '\xa0'] { token lexbuf}
  | line_terminator { token lexbuf}
  | _ as c {CHAR(c)}

and identifier buf = parse
    | reserved_word { to_keyword (Lexing.lexeme lexbuf)}
    | _ {identifier_name buf lexbuf}
and identifier_name buf = parse
    | identifier_start {identifier_part (buf ^ (Lexing.lexeme lexbuf)) lexbuf}
    | white_space | line_terminator {IDENT(buf)}
and identifier_part buf = parse
    | identifier_start | ['0'-'9'] {identifier_part (buf ^ (Lexing.lexeme lexbuf)) lexbuf}
    | white_space | line_terminator {IDENT(buf)}
and single_line_comment buf = parse
    | line_terminator {SINGLE_LINE_COMMENT(buf)}
    | _* {single_line_comment (buf ^ (Lexing.lexeme lexbuf)) lexbuf}
and multi_line_comment buf = parse
    | "*/" {MULTI_LINE_COMMENT(buf)}
    | [^ '*' '/']* {multi_line_comment (buf ^ (Lexing.lexeme lexbuf)) lexbuf}
