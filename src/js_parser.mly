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

(* punctuators *)
%token LPAREN LBRACE LCBRACE RPAREN RBRACE RCBRACE COLON COMMA SEMICOLON LESS
%token GREATER LESS_THAN GREATER_THAN EQUAL NOT_EQUAL DEEP_EQUAL DEEP_NOT_EQUAL PLUS
%token MINUS MULTI MOD INCREMENT DECREMENT LSHIFT RSHIFT AND OR XOR NOT COMP
%token LOGICAL_AND LOGICAL_OR QUESTION ASSIGN PLUS_ASSIGN MINUS_ASSIGN MULTI_ASSIGN
%token MOD_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN  DOT
%token DIV DIV_ASSIGN

(* keyword *)
%token KEYWORD_THIS
%token KEYWORD_GET KEYWORD_SET KEYWORD_NEW KEYWORD_IN KEYWORD_INSTANCEOF
%token KEYWORD_DELETE KEYWORD_TYPEOF KEYWORD_FUNCTION
%token KEYWORD_VOID KEYWORD_VAR KEYWORD_IF KEYWORD_ELSE KEYWORD_DO
%token KEYWORD_WHILE KEYWORD_FOR KEYWORD_CONTINUE KEYWORD_BREAK
%token KEYWORD_RETURN KEYWORD_WITH KEYWORD_SWITCH KEYWORD_CASE KEYWORD_DEFAULT
%token KEYWORD_THROW KEYWORD_TRY KEYWORD_CATCH KEYWORD_FINALLY KEYWORD_DEBUGGER

%token TRUE FALSE NULL
%token DOLLAR UNDERSCORE
%token DOUBLE_QUOTE SINGLE_QUOTE
%token <char> EXP
%token <string> DIGIT
%token <char> CHAR
%token EOF
%token <string> CONTROL_CHAR
%token <string> SINGLE_CONTROL_CHAR
%start parser_main
%type <Js_type.t> parser_main
%%

parser_main:
program EOF {$1}
  ;

  (* Expression grammers *)

  primary_expression:
    KEYWORD_THIS { Js_type.Jexp_this }
   | identifier { $1 }
   | literal {Js_type.Jexp_literal($1)}
   | array_literal {Js_type.Jexp_array($1)}
   | object_literal {Js_type.Jexp_object($1)}
   | LPAREN expression RPAREN {$1}
  ;

  array_literal:
    LBRACE RBRACE { [] }
   |LBRACE elision RBRACE {[$2]}
   |LBRACE element_list RBRACE {$2}
  ;

  element_list:
    assignment_expression { [$1] }
   |elision assignment_expression { [$2] }
   |element_list COMMA assignment_expression { $3 @ $1 }
   |element_list COMMA elision assignment_expression { $3 @ $1 }
  ;

  elision:
    COMMA {[Js_type.Jexp_literal(Js_type.Jl_null)]}
   |elision COMMA {elision @ [ Js_type.Jexp_literal(Js_type.Jl_null)] }
  ;

  object_literal:
    LCBRACE RCBRACE {[]}
   |LCBRACE property_name_and_value_list RCBRACE {$2}
   |LCBRACE property_name_and_value_list COMMA RCBRACE {$2}
  ;

  property_name_and_value_list:
    property_assignment {[$1]}
   |property_name_and_value_list COMMA property_assignment {$1 @ [$3]}
  ;

  property_assignment:
    property_name COLON assignment_expression {Js_type.Jexp_property ($1,$3)}
   |KEYWORD_GET property_name LPAREN RPAREN LCBRACE function_body RCBRACE {
     Js_type.Jexp_property ($2, Js_type.Jexp_function ([], $6))
   }
   |KEYWORD_SET property_name LPAREN property_set_parameter_list RPAREN LCBRACE function_body RCBRACE {
     Js_type.Jexp_property ($2, Js_type.Jexp_function ($4, $7))
   }
  ;

  property_name:
    identifier {$1}
   |string {$1}
   |number {$1}
  ;

  property_set_parameter_list:
    identifier {$1}
  ;

  member_expression:
    primary_expression {$1}
   |function_expression {$1}
   |member_expression LBRACE expression RBRACE {Js_type.Jexp_member($1, $3)}
   |member_expression DOT identifier {Js_type.Jexp_member($1, $3)}
   |KEYWORD_NEW member_expression arguments {Js_type.Jexp_new($2, $3)}
  ;

  new_expression:
    member_expression {$1}
   |KEYWORD_NEW new_expression {$2}
  ;

  call_expression:
    member_expression arguments {Js_type.Jexp_call($1,$2)}
   |call_expression arguments {Js_type.Jexp_call($1, $2)}
   |call_expression LBRACE arguments RBRACE {Js_type.Jexp_call($1, $3)}
   |call_expression DOT identifier {Js_type.Jexp_call($1,[$3])}
  ;

  arguments:
    LPAREN RPAREN {[]}
   |LPAREN argument_list RPAREN {$2}
  ;

  argument_list:
    assignment_expression {$1}
   |argument_list COMMA assignment_expression {$1 @ $3}
  ;

  left_hand_side_expression:
    new_expression {$1}
   |call_expression {$1}
  ;

  post_fix_expresison:
    left_hand_side_expression {$1}
   |left_hand_side_expression INCREMENT {Js_type.Jexp_update($1, $2, true)}
   |left_hand_side_expression DECREMENT {Js_type.Jexp_update($1, $2, true)}
  ;

  unary_expression:
    post_fix_expresison {$1}
   |KEYWORD_DELETE unary_expression {Js_type.Jexp_unary($2, $1)}
   |KEYWORD_VOID unary_expression {Js_type.Jexp_unary($2, $1)}
   |KEYWORD_TYPEOF unary_expression {Js_type.Jexp_unary($2, $1)}
   |INCREMENT unary_expression {Js_type.Jexp_update($2, $1, false)}
   |DECREMENT unary_expression {Js_type.Jexp_update($2, $1, false)}
   |PLUS unary_expression {Js_type.Jexp_unary($2, $1)}
   |MINUS unary_expression {Js_type.Jexp_unary($2, $1)}
   |COMP unary_expression {Js_type.Jexp_unary($2, $1)}
   |NOT unary_expression {Js_type.Jexp_unary($2, $1)}
  ;

  multiplicative_expression:
    unary_expression {$1}
   |multiplicative_expression MULTI unary_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |multiplicative_expression MOD unary_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |multiplicative_expression DIV unary_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  additive_expression:
    multiplicative_expression {$1}
   |additive_expression PLUS multiplicative_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |additive_expression MINUS multiplicative_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  shift_expression:
    additive_expression {$1}
   |shift_expression LSHIFT additive_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |shift_expression RSHIFT additive_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  relational_expression:
    shift_expression {$1}
   |relational_expression LESS shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |relational_expression GREATER shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |relational_expression LESS_THAN shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |relational_expression GREATER_THAN shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |relational_expression KEYWORD_INSTANCEOF shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |relational_expression KEYWORD_IN shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  relational_expression_no_in:
    shift_expression {$1}
   |relational_expression LESS shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |relational_expression GREATER shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |relational_expression LESS_THAN shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |relational_expression GREATER_THAN shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |relational_expression KEYWORD_INSTANCEOF shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  equality_expression:
    relational_expression {$1}
   |equality_expression EQUAL relational_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |equality_expression NOT_EQUAL relational_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |equality_expression DEEP_EQUAL relational_expression {Js_type.Jexp_binary ($1, $2, $3)}
   |equality_expression DEEP_NOT_EQUAL relational_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  equality_expression_no_in:
    relational_expression_no_in {$1}
   |equality_expression_no_in EQUAL relational_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
   |equality_expression_no_in NOT_EQUAL relational_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
   |equality_expression_no_in DEEP_EQUAL relational_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
   |equality_expression_no_in DEEP_NOT_EQUAL relational_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  bitwise_and_expression:
    equality_expression {$1}
   |bitwise_and_expression AND equality_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  bitwise_and_expression_no_in:
    equality_expression_no_in {$1}
   |bitwise_and_expression_no_in AND equality_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  bitwise_xor_expression:
    bitwise_and_expression {$1}
   |bitwise_xor_expression XOR bitwise_and_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  bitwise_xor_expression_no_in:
    bitwise_and_expression_no_in {$1}
   |bitwise_xor_expression_no_in XOR bitwise_and_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  bitwise_or_expression:
    bitwise_xor_expression {$1}
   |bitwise_or_expression OR bitwise_xor_expression {Js_type.Jexp_binary($1, $2, $3)}
  ;

  bitwise_or_expression_no_in:
    bitwise_xor_expression_no_in {$1}
   |bitwise_or_expression_no_in OR bitwise_xor_expression_no_in {Js_type.Jexp_binary($1, $2, $3)}
  ;

  logical_and_expression:
    bitwise_or_expression {$1}
   |logical_and_expression LOGICAL_AND bitwise_or_expression {Js_type.Jexp_binary($1, $2, $3)}
  ;
  logical_and_expression_no_in:
    bitwise_or_expression_no_in {$1}
   |logical_and_expression_no_in LOGICAL_AND bitwise_or_expression_no_in {Js_type.Jexp_binary($1, $2, $3)}
  ;

  logical_or_expression:
    logical_and_expression {$1}
   |logical_or_expression LOGICAL_OR logical_and_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;
  logical_or_expression_no_in:
    logical_and_expression_no_in {$1}
   |logical_or_expression_no_in LOGICAL_OR logical_and_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  conditional_expression:
    logical_or_expression {$1}
   |logical_or_expression QUESTION assignment_expression COLON assignment_expression {Js_type.Jexp_conditional($1, $2, $3)}
  ;
  conditional_expression_no_in:
    logical_or_expression_no_in {$1}
   |logical_or_expression_no_in QUESTION assignment_expression COLON assignment_expression
       {Js_type.Jexp_conditional ($1, $2, $3)}
  ;

  assignment_expression:
    conditional_expression {$1}
   |left_hand_side_expression assignment_operator assignment_expression {Js_type.Jexp_assignment($1, $2, $3)}
  ;
  assignment_expression_no_in:
    conditional_expression_no_in {$1}
   |left_hand_side_expression assignment_operator assignment_expression_no_in {Js_type.Jexp_assignment($1, $2, $3)}
  ;

  assignment_operator:
    ASSIGN  {$1}
   |PLUS_ASSIGN  {$1}
   |MINUS_ASSIGN  {$1}
   |MULTI_ASSIGN  {$1}
   |MOD_ASSIGN  {$1}
   |LSHIFT_ASSIGN  {$1}
   |RSHIFT_ASSIGN  {$1}
   |AND_ASSIGN  {$1}
   |OR_ASSIGN  {$1}
   |XOR_ASSIGN  {$1}
  ;

  expression:
    assignment_expression {[$1]}
   |expression COMMA assignment_expression {$1 @ [$3]}
  ;

  expression_no_in:
    assignment_expression_no_in {[$1]}
   |expression_no_in COMMA assignment_expression_no_in {$1 @ [$3]}
  ;

  (* --- Expression grammers *)

  (* Statement grammers *)

  block:
   |LCBRACE loption(statement) RCBRACE {$2}
  ;

  statement:
    block                {Js_type.Jstm_block($1)}
   |variable_statement   {$1}
   |empty_statement      {$1}
   |expression_statement {$1}
   |if_statement         {$1}
   |iteration_statement  {$1}
   |continue_statement   {$1}
   |break_statement      {$1}
   |return_statement     {$1}
   |with_statement       {$1}
   |labelled_statement   {$1}
   |switch_statement     {$1}
   |throw_statement      {$1}
   |try_statement        {$1}
   |debugger_statement   {$1}
  ;

  variable_statement:
    KEYWORD_VAR variable_declaration_list SEMICOLON {Js_type.Jstm_var($2)}
  ;

  variable_declaration_list:
    variable_declaration {[$1]}
   |variable_declaration_list COMMA variable_declaration {$1 @ [$3]}
  ;
  variable_declaration_list_no_in:
    variable_declaration_no_in {[$1]}
   |variable_declaration_list_no_in COMMA variable_declaration_no_in {$1 @ [$3]}
  ;

  variable_declaration:
    identifier {Jdec_var($1, None)}
   |identifier initialiser {Jdec_var($1, Some($2))}
  ;
  variable_declaration_no_in:
    identifier {Jdec_var($1, None)}
   |identifier initialiser_no_in {Jdec_var($1, Some($2))}
  ;

  initialiser:
    ASSIGN assignment_expression {$2}
  ;
  initialiser_no_in:
    ASSIGN assignment_expression_no_in {$2}
  ;

  empty_statement:
    SEMICOLON {}
  ;

  expression_statement:
    expression {Jstm_expression($1)}
  ;

  if_statement:
    KEYWORD_IF LPAREN expression RPAREN statement {Js_type.Jstm_if($3, $5, None)}
   |KEYWORD_IF LPAREN expression RPAREN statement KEYWORD_ELSE statement {Js_type.Jstm_if($3, $5, Some(7))}
  ;

  iteration_statement:
    KEYWORD_DO statement KEYWORD_WHILE LPAREN expression RPAREN SEMICOLON {
      Js_type.Jstm_do_while($5, $2)}
   |KEYWORD_WHILE LPAREN expression RPAREN statement {Js_type.Jstm_while($3, $5)}
   |KEYWORD_FOR LPAREN option(expression_no_in) SEMICOLON
       option(expression)
       SEMICOLON option(expression) RPAREN statement {Js_type.Jstm_for($3, $5, $7, $9)}
   |KEYWORD_FOR LPAREN KEYWORD_VAR variable_declaration_list_no_in SEMICOLON
       option(expression)
       SEMICOLON option(expression) RPAREN statement {Js_type.Jstm_for_dec($4, $6, $8, $10)}
   |KEYWORD_FOR LPAREN left_hand_side_expression KEYWORD_IN expression RPAREN statement {
     Js_type.Jstm_for_in($3, $5, $7)}
   |KEYWORD_FOR LPAREN KEYWORD_VAR variable_declaration_no_in KEYWORD_IN expression RPAREN statement {
     Js_type.Jstm_for_in_dec($4, $6, $8)}
  ;

  continue_statement:
    KEYWORD_CONTINUE SEMICOLON {Js_type.Jstm_continue(None)}
   |KEYWORD_CONTINUE identifier SEMICOLON {Js_type.Jstm_continue(Some($2))}
  ;

  break_statement:
    KEYWORD_BREAK SEMICOLON {Js_type.Jstm_break(None)}
   |KEYWORD_BREAK identifier SEMICOLON {Js_type.Jstm_break(Some($2))}
  ;

  return_statement:
    KEYWORD_RETURN SEMICOLON {Js_type.Jstm_return (None)}
   |KEYWORD_RETURN expression SEMICOLON {Js_type.Jstm_return(Some($2))}
  ;

  with_statement:
    KEYWORD_WITH LPAREN expression RPAREN statement {Js_type.Jstm_with($3, $4)}
  ;

  switch_statement:
    KEYWORD_SWITCH LPAREN expression RPAREN case_block {Js_type.Jstm_switch($3, $4)}
  ;

  case_block:
    LCBRACE loption(case_clause) RCBRACE {$2}
   |LCBRACE loption(case_clause) default_clause loption(case_clause) RCBRACE {$2 @ [$3] @ $4}
  ;

  case_clause:
    KEYWORD_CASE expression COLON loption(statement) {Js_type.Jcas_case(Some($2), $4)}
  ;

  default_clause:
    KEYWORD_DEFAULT COLON loption(statement) {Js_type.Jcas_case(None, $3)}
  ;

  labelled_statement:
    identifier COLON statement {Js_type.Jstm_labelled ($1, $3)}
  ;

  throw_statement:
    KEYWORD_THROW expression SEMICOLON {Js_type.Jstm_throw ($2)}
  ;

  try_statement:
    KEYWORD_TRY block catch {Js_type.Jstm_try($2, Some($3), None)}
   |KEYWORD_TRY block finally {Js_type.Jstm_try($2, None, Some($3))}
   |KEYWORD_TRY block catch finally {Js_type.Jstm_try($2, Some($3), Some($4))}
  ;

  catch:
    KEYWORD_CATCH LPAREN identifier RPAREN block {Js_type.Jstm_catch($3, $5)}
  ;

  finally:
    KEYWORD_FINALLY block {Js_type.Jstm_finally ($2)}
  ;

  debugger_statement:
    KEYWORD_DEBUGGER SEMICOLON {Js_type.Jstm_debugger()}
  ;

  (* --- Statement grammers *)

  (* Programs and functions grammer *)

  function_declaration:
    KEYWORD_FUNCTION identifier LPAREN loption(formal_parameter) RPAREN LCBRACE function_body RCBRACE {
      Js_type.Jdec_function ($2, $4, $7)
    }
  ;

  function_expression:
    KEYWORD_FUNCTION option(identifier) LPAREN loption(formal_parameter) RPAREN LCBRACE function_body RCBRACE {
      Js_type.Jexp_function ($2, $4, $7)
    }
  ;

  formal_parameter:
    identifier {$1}
  ;

  function_body:
    loption(source_element) {$1}
  ;

  source_element:
    statement {$1}
   |function_declaration {$1}
  ;

  program:
    loption(source_element) {Js_type.Jprog_program ($1)}
  ;

  (* --- Programs and functions grammer *)

  identifier:
    identifier_name {Js_type.Jexp_ident($1)}
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

  literal:
    NULL {Js_type.Jl_null}
   |TRUE {Js_type.Jl_bool(true)}
   |FALSE {Js_type.Jl_bool(false)}
   |string {Js_type.Jl_string($1)}
   |number {Js_type.Jl_number($1)}
  ;

  string:
    DOUBLE_QUOTE DOUBLE_QUOTE  { "" }
   | SINGLE_QUOTE SINGLE_QUOTE  { "" }
   | DOUBLE_QUOTE double_chars DOUBLE_QUOTE  { $2 }
   | SINGLE_QUOTE single_chars SINGLE_QUOTE  { $2 }
  ;

  double_chars:
    double_char  { $1 }
   | double_char double_chars { $1 ^ $2 }
  ;

  single_chars:
    single_char  { $1 }
   | single_char single_chars { $1 ^ $2 }
  ;

  punctuator_chars:
    DOLLAR {"$"}
   |UNDERSCORE {"_"}
   |LCBRACE {"{"}
   |LPAREN {"("}
   |LBRACE {"["}
   |RCBRACE {"}"}
   |RPAREN {")"}
   |RBRACE {"]"}
   |COLON {":"}
   |SEMICOLON {";"}
   |COMMA {","}
   |MINUS {"-"}
   |PLUS {"+"}
   |LESS {"<"}
   |GREATER {">"}
   |LESS_THAN {"<="}
   |GREATER_THAN {">="}
   |EQUAL {"=="}
   |NOT_EQUAL {"!="}
   |DEEP_EQUAL {"==="}
   |DEEP_NOT_EQUAL {"!=="}
   |MULTI {"*"}
   |MOD {"%"}
   |INCREMENT {"++"}
   |DECREMENT {"--"}
   |LSHIFT {"<<"}
   |RSHIFT {">>"}
   |AND {"&"}
   |OR {"|"}
   |XOR {"^"}
   |NOT {"!"}
   |COMP {"~"}
   |LOGICAL_AND {"&&"}
   |LOGICAL_OR {"||"}
   |QUESTION {"?"}
   |ASSIGN {"="}
   |PLUS_ASSIGN {"+="}
   |MINUS_ASSIGN {"-="}
   |MULTI_ASSIGN {"*="}
   |MOD_ASSIGN {"%="}
   |LSHIFT_ASSIGN {"<<="}
   |RSHIFT_ASSIGN {">>="}
   |AND_ASSIGN {"&="}
   |OR_ASSIGN {"|="}
   |XOR_ASSIGN {"^="}
   |DIV {"/"}
   |DIV_ASSIGN {"/="}
  ;

  double_char:
    CONTROL_CHAR   { $1 }
   | DIGIT         { $1 }
   | EXP           { Char.escaped($1) }
   | CHAR          { Char.escaped($1) }
   | punctuator_chars {$1}
  ;

  single_char:
    SINGLE_CONTROL_CHAR   { $1 }
   | DIGIT         { $1 }
   | EXP           { Char.escaped($1) }
   | punctuator_chars {$1}
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
