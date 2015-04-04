{
  open Json_parser
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
    | "==" { equal}
    | "!=" {not_equal}
    | "===" {deep_equal}
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
    | ['0'-'9']+ as digit { DIGIT(digit) }
    | '.' { DOT }
    | '"' { DOUBLE_QUOTE }
    | ['e' 'E'] as exp { EXP(exp) }
    | '\\' ['"' '\\' '/' 'b' 'f' 'n' 'r' 't']  as ctl { CONTROL_CHAR(ctl) }
    | "\\u"['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F'] as ctl { CONTROL_CHAR(ctl) }
    | eof  {EOF}
    | _ as c { CHAR(c) }
