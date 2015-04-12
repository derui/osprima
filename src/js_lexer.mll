{
  open Js_parser

  exception SyntaxError of string
}
(* Json Tokens *)
rule token = parse
    [' ' '\t' '\x0b' '\x0c' '\xa0']    { token lexbuf }
    | ['\n' '\r'] | "\r\n" | "\x20\x28" | "\x20\x29" { token lexbuf }
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
    | ['0'-'9']+ as digit { DIGIT(digit) }
    | '.' { DOT }
    | '"' { DOUBLE_QUOTE }
    | '\'' { SINGLE_QUOTE }
    | ['e' 'E'] as exp { EXP(exp) }
    | '\\' ['"' '\\' '/' 'b' 'f' 'n' 'r' 't']  as ctl { CONTROL_CHAR(ctl) }
    | '\\' ['\'' '\\' '/' 'b' 'f' 'n' 'r' 't']  as ctl { SINGLE_CONTROL_CHAR(ctl) }
    | "\\u"['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F'] as ctl { CONTROL_CHAR(ctl) }
    | eof  {EOF}
    | _ as c { CHAR(c) }
