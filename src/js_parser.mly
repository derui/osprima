/* JavaScriptのパーサー */
%{
  open CamomileLibrary
%}

(* %{ *)
(*   let to_str = function *)
(*     | Json_type.String(s) -> "string:" ^ s *)
(*     | Json_type.Number(_) -> "number" *)
(*     | Json_type.Object(_) -> "object" *)
(*     | Json_type.Array(_) -> "array" *)
(*     | Json_type.Null -> "null" *)
(*     | Json_type.Bool(_) -> "bool" *)
(* %} *)

%token WHITESPACE
%token LINE_TERMINATOR
%token <identifier> IDENT
%token <string> STRING

/* punctuators */
%token LPAREN LBRACE LCBRACE RPAREN RBRACE RCBRACE COLON COMMA SEMICOLON LESS
%token GREATER LESS_THAN GREATER_THAN EQUAL NOT_EQUAL DEEP_EQUAL DEEP_NOT_EQUAL PLUS
%token MINUS MULTI MOD INCREMENT DECREMENT LSHIFT RSHIFT AND OR XOR NOT COMP
%token LOGICAL_AND LOGICAL_OR QUESTION ASSIGN PLUS_ASSIGN MINUS_ASSIGN MULTI_ASSIGN  
%token MOD_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN  DOT
%token DIV DIV_ASSIGN

%token TRUE FALSE NULL
%token DOLLAR UNDERSCORE
%token DOUBLE_QUOTE
%token <char> EXP
%token <string> DIGIT
%token <char> CHAR
%token EOF
%token <string> CONTROL_CHAR
%token <float> NUMBER
%token <bool> BOOL
%start parser_main
%type <Js_type.t> parser_main
%%

parser_main:
value EOF {$1}
  ;
  
  identifier:
   identifier_name {Js_type.Ident($1) }
  ;

  identifier_name:
    identifier_start {$1}
   |identifier_name identifier_part {$1 ^ $2}
  ;

  identifier_start:
    CHAR          { Char.escaped($1) }
   | UNDERSCORE   { $1 }
   | DOLLAR   { $1 }
  ;

  identifier_part:
    identifier_start {$1}
   | DIGIT  { $1 }
  ;

  punctuator:
    LPAREN {$1}
   |LBRACE {$1}
   |LCBRACE {$1}
   |RPAREN {$1}
   |RBRACE {$1}
   |RCBRACE {$1}
   |COLON  {$1}
   |COMMA  {$1}
   |SEMICOLON  {$1}
   |LESS  {$1}
   |GREATER  {$1}
   |LESS_THAN  {$1}
   |GREATER_THAN  {$1}
   |EQUAL  {$1}
   |NOT_EQUAL  {$1}
   |DEEP_EQUAL  {$1}
   |DEEP_NOT_EQUAL  {$1}
   |PLUS  {$1}
   |MINUS  {$1}
   |MULTI  {$1}
   |MOD  {$1}
   |INCREMENT  {$1}
   |DECREMENT  {$1}
   |LSHIFT  {$1}
   |RSHIFT  {$1}
   |AND   {$1}
   |OR  {$1}
   |XOR  {$1}
   |NOT  {$1}
   |COMP  {$1}
   |LOGICAL_AND  {$1}
   |LOGICAL_OR  {$1}
   |QUESTION  {$1}
   |ASSIGN  {$1}
   |PLUS_ASSIGN  {$1}
   |MINUS_ASSIGN  {$1}
   |MULTI_ASSIGN  {$1}
   |MOD_ASSIGN  {$1}
   |LSHIFT_ASSIGN  {$1}
   |RSHIFT_ASSIGN  {$1}
   |AND_ASSIGN  {$1}
   |OR_ASSIGN  {$1}
   |XOR_ASSIGN  {$1}
   |DOT {$1}
  ;

  div_punctuator:
    DIV {$1}
   |DIV_ASSIGN {$1}
  ;

  literal:
    NULL {Js_type.Literal.Null}
   |TRUE {Js_type.Literal.Bool(true)}
   |FALSE {Js_type.Literal.Bool(false)}
   |string {Js_type.Literal.String($1)}
   |number {Js_type.Literal.Number($1)}
  ;

  value:
    string     { Json_type.String($1) }
   | number    { Json_type.Number($1) }
   | array     { Json_type.Array($1) }
   | obj       { Json_type.Object($1) }
   | TRUE      { Json_type.Bool(true) }
   | FALSE     { Json_type.Bool(false) }
   | NULL      { Json_type.Null }
  ;

  obj:
    LPAREN RPAREN  { [] }
   | LPAREN members RPAREN {$2 }
  ;

  members:
    pair           { [$1] }
   | pair COMMA members { $1 :: $3 }
  ;

  pair:
    string COLON value { ($1, $3) }
  ;

  array:
    LBRACE RBRACE  { [] }
   | LBRACE elements RBRACE  { $2 }
  ;

  elements:
    value    { [$1] }
   | value COMMA elements  { $1 :: $3 }
  ;

  string:
    DOUBLE_QUOTE DOUBLE_QUOTE  { "" }
   | DOUBLE_QUOTE chars DOUBLE_QUOTE  { $2 }
  ;

  chars:
    char  { $1 }
   | char chars { $1 ^ $2 }
  ;

  char:
    CONTROL_CHAR   { $1 }
   | DIGIT         { $1 }
   | EXP           { Char.escaped($1) }
   | DOT           { "." }
   | MINUS         { "-" }
   | PLUS          { "+" }
   | LBRACE        { "{" }
   | RBRACE        { "}" }
   | LPAREN        { "(" }
   | RPAREN        { ")" }
   | CHAR          { Char.escaped($1) }
  ;
  number:
    integer       { float_of_string($1) }
   | integer frac  { float_of_string($1 ^ $2) }
   | integer exp   { float_of_string($1 ^ $2) }
   | integer frac exp { float_of_string($1 ^ $2 ^ $3) }
  ;

  integer:
    DIGIT   { $1 }
   | DIGIT digits  { $1 ^ $2 }
   | MINUS DIGIT { "-" ^ $2 }
   | MINUS DIGIT digits { "-" ^ $2 ^ $3 }
  ;

  frac:
    DOT digits { "." ^ $2 }
  ;

  exp:
    e digits { "e" ^ $2 }
  ;

  digits:
    DIGIT  { $1 }
   |DIGIT digits { $1 ^ $2 }
  ;

  e:
    EXP  { Char.escaped($1) }
   | EXP PLUS { Char.escaped($1) ^ "+"}
   | EXP MINUS { Char.escaped($1) ^ "-" }
  ;
