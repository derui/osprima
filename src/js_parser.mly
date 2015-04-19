/* JavaScriptのパーサー */
%{
  open CamomileLibrary
  exception SyntaxError of string
%}

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

(* %token <string> WHITE_SPACE *)
(* %token <string> LINE_TERMINATOR *)

%token <string> IDENT
%token <string> MULTI_LINE_COMMENT
%token <string> SINGLE_LINE_COMMENT
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
%type <Js_type.program option> parser_main
%%
  ;

  parser_main:
    EOF {None}
   |program EOF {Some $1}
  ;

  (* Expression grammers *)

  primary_expression:
    KEYWORD_THIS { Js_type.Jexp_this }
   | identifier { $1 }
   | literal {Js_type.Jexp_literal($1)}
   | array_literal {Js_type.Jexp_array($1)}
   | object_literal {Js_type.Jexp_object($1)}
   | LPAREN expression RPAREN {$2}
  ;

  array_literal:
    LBRACE RBRACE { [] }
   |LBRACE elision RBRACE {$2}
   |LBRACE element_list RBRACE {$2}
  ;

  element_list:
    assignment_expression { [$1] }
   |elision assignment_expression { [$2] }
   |element_list COMMA assignment_expression { $1 @ [$3] }
   |element_list COMMA elision assignment_expression { $1 @ [$4] }
  ;

  elision:
    COMMA {[Js_type.Jexp_literal(Js_type.Jl_null)]}
   |elision COMMA {$1 @ [Js_type.Jexp_literal(Js_type.Jl_null)] }
  ;

  object_literal:
    LCBRACE RCBRACE {[]}
   |LCBRACE list(property_assignment) option(COMMA) RCBRACE {$2}
  ;

  property_assignment:
    property_name COLON assignment_expression {Js_type.Jexp_property ($1,$3)}
   |KEYWORD_GET property_name LPAREN RPAREN LCBRACE function_body RCBRACE {
     Js_type.Jexp_property ($2, Js_type.Jexp_function (None, [], Js_type.Jstm_block($6)))
   }
   |KEYWORD_SET property_name LPAREN property_set_parameter_list RPAREN LCBRACE function_body RCBRACE {
     Js_type.Jexp_property ($2, Js_type.Jexp_function (None, $4, Js_type.Jstm_block($7)))
   }
  ;

  property_name:
    identifier_name {Js_type.Jexp_ident ($1)}
   |string {Js_type.Jexp_literal($1)}
   |number {Js_type.Jexp_literal($1)}
  ;

  property_set_parameter_list:
    list(identifier) {$1}
  ;

  member_expression:
    primary_expression {$1}
   |function_expression {$1}
   |member_expression LBRACE expression RBRACE {Js_type.Jexp_member($1, $3)}
   |member_expression DOT identifier {Js_type.Jexp_member($1, $3)}
   |KEYWORD_NEW member_expression arguments {Js_type.Jexp_new($2, $3)}
  ;

  call_expression:
    member_expression arguments {Js_type.Jexp_call($1,$2)}
   |call_expression arguments {Js_type.Jexp_call($1, $2)}
   |call_expression LBRACE expression RBRACE {Js_type.Jexp_member($1, $3)}
   |call_expression DOT identifier {Js_type.Jexp_member($1,$3)}
  ;

  arguments:
    LPAREN list=separated_list(COMMA, assignment_expression) RPAREN {list}
  ;

  left_hand_side_expression:
    member_expression {$1}
    |call_expression {$1}
  ;

  post_fix_expresison:
    left_hand_side_expression {$1}
                |left_hand_side_expression tok_increment {Js_type.Jexp_update($1, $2, true)}
                |left_hand_side_expression tok_decrement {Js_type.Jexp_update($1, $2, true)}
  ;

  unary_expression:
    post_fix_expresison {$1}
                |keyword_delete unary_expression {Js_type.Jexp_unary($2, $1)}
                |keyword_void unary_expression {Js_type.Jexp_unary($2, $1)}
                |keyword_typeof unary_expression {Js_type.Jexp_unary($2, $1)}
                |tok_increment unary_expression {Js_type.Jexp_update($2, $1, false)}
                |tok_decrement unary_expression {Js_type.Jexp_update($2, $1, false)}
                |tok_plus unary_expression {Js_type.Jexp_unary($2, $1)}
                |tok_minus unary_expression {Js_type.Jexp_unary($2, $1)}
                |tok_comp unary_expression {Js_type.Jexp_unary($2, $1)}
                |tok_not unary_expression {Js_type.Jexp_unary($2, $1)}
  ;

  multiplicative_expression:
    unary_expression {$1}
                |multiplicative_expression tok_multi unary_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |multiplicative_expression tok_mod unary_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |multiplicative_expression tok_div unary_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  additive_expression:
    multiplicative_expression {$1}
                |additive_expression tok_plus multiplicative_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |additive_expression tok_minus multiplicative_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  shift_expression:
    additive_expression {$1}
                |shift_expression tok_lshift additive_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |shift_expression tok_rshift additive_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  relational_expression:
    shift_expression {$1}
                |relational_expression tok_less shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |relational_expression tok_greater shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |relational_expression tok_less_than shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |relational_expression tok_greater_than shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |relational_expression keyword_instanceof shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |relational_expression keyword_in shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  relational_expression_no_in:
    shift_expression {$1}
                |relational_expression tok_less shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |relational_expression tok_greater shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |relational_expression tok_less_than shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |relational_expression tok_greater_than shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |relational_expression keyword_instanceof shift_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  equality_expression:
    relational_expression {$1}
                |equality_expression tok_equal relational_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |equality_expression tok_not_equal relational_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |equality_expression tok_deep_equal relational_expression {Js_type.Jexp_binary ($1, $2, $3)}
                |equality_expression tok_deep_not_equal relational_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  equality_expression_no_in:
    relational_expression_no_in {$1}
                |equality_expression_no_in tok_equal relational_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
                |equality_expression_no_in tok_not_equal relational_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
                |equality_expression_no_in tok_deep_equal relational_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
                |equality_expression_no_in tok_deep_not_equal relational_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  bitwise_and_expression:
    equality_expression {$1}
                |bitwise_and_expression tok_and equality_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  bitwise_and_expression_no_in:
    equality_expression_no_in {$1}
                |bitwise_and_expression_no_in tok_and equality_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  bitwise_xor_expression:
    bitwise_and_expression {$1}
                |bitwise_xor_expression tok_xor bitwise_and_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  bitwise_xor_expression_no_in:
    bitwise_and_expression_no_in {$1}
                |bitwise_xor_expression_no_in tok_xor bitwise_and_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  bitwise_or_expression:
    bitwise_xor_expression {$1}
                |bitwise_or_expression tok_or bitwise_xor_expression {Js_type.Jexp_binary($1, $2, $3)}
  ;

  bitwise_or_expression_no_in:
    bitwise_xor_expression_no_in {$1}
                |bitwise_or_expression_no_in tok_or bitwise_xor_expression_no_in {Js_type.Jexp_binary($1, $2, $3)}
  ;

  logical_and_expression:
    bitwise_or_expression {$1}
                |logical_and_expression tok_logical_and bitwise_or_expression {Js_type.Jexp_binary($1, $2, $3)}
  ;
  logical_and_expression_no_in:
    bitwise_or_expression_no_in {$1}
                |logical_and_expression_no_in tok_logical_and bitwise_or_expression_no_in {Js_type.Jexp_binary($1, $2, $3)}
  ;

  logical_or_expression:
    logical_and_expression {$1}
                |logical_or_expression tok_logical_or logical_and_expression {Js_type.Jexp_binary ($1, $2, $3)}
  ;
  logical_or_expression_no_in:
    logical_and_expression_no_in {$1}
                |logical_or_expression_no_in tok_logical_or logical_and_expression_no_in {Js_type.Jexp_binary ($1, $2, $3)}
  ;

  conditional_expression:
    logical_or_expression {$1}
                |logical_or_expression QUESTION assignment_expression COLON assignment_expression {Js_type.Jexp_conditional($1, $3, $5)}
  ;
  conditional_expression_no_in:
    logical_or_expression_no_in {$1}
                |logical_or_expression_no_in QUESTION assignment_expression COLON assignment_expression
                    {Js_type.Jexp_conditional ($1, $3, $5)}
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
    tok_assign  {$1}
                |tok_plus_assign  {$1}
                |tok_minus_assign  {$1}
                |tok_multi_assign  {$1}
                |tok_mod_assign  {$1}
                |tok_lshift_assign  {$1}
                |tok_rshift_assign  {$1}
                |tok_and_assign  {$1}
                |tok_or_assign  {$1}
                |tok_xor_assign  {$1}
  ;

  expression:
    exp=separated_nonempty_list(COMMA,assignment_expression) {Js_type.Jexp_sequence(exp)}
  ;

  expression_no_in:
    exp=separated_nonempty_list(COMMA,assignment_expression_no_in) {Js_type.Jexp_sequence(exp)}
  ;

  (* --- Expression grammers *)

  (* Statement grammers *)

  block:
        |LCBRACE list(statement) RCBRACE {Js_type.Jstm_block($2)}
  ;

  statement:
    block                {$1}
        |variable_statement   {$1}
        |empty_statement      {$1}
        |expression_statement {$1}
        |if_statement         {$1}
        |if_else_statement    {$1}
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
    KEYWORD_VAR separated_nonempty_list(COMMA,variable_declaration) SEMICOLON {Js_type.Jstm_var($2)}
  ;

  variable_declaration_list_no_in:
    separated_nonempty_list(COMMA, variable_declaration_no_in) {$1}
  ;

  variable_declaration:
    identifier {Js_type.Jdec_var($1, None)}
        |identifier initialiser {Js_type.Jdec_var($1, Some($2))}
  ;
  variable_declaration_no_in:
    identifier {Js_type.Jdec_var($1, None)}
        |identifier initialiser_no_in {Js_type.Jdec_var($1, Some($2))}
  ;

  initialiser:
    ASSIGN assignment_expression {$2}
  ;
  initialiser_no_in:
    ASSIGN assignment_expression_no_in {$2}
  ;

  empty_statement:
    SEMICOLON {Js_type.Jstm_empty}
  ;

  expression_statement:
    expression {Js_type.Jstm_expression($1)}
  ;

  if_statement:
    KEYWORD_IF LPAREN expression RPAREN statement {Js_type.Jstm_if($3, $5)}
  ;

  if_else_statement:
    stm=if_statement KEYWORD_ELSE statement {Js_type.Jstm_if_else(stm, $3)}
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
    KEYWORD_WITH LPAREN expression RPAREN statement {Js_type.Jstm_with($3, $5)}
  ;

  switch_statement:
    KEYWORD_SWITCH LPAREN expression RPAREN case_block {Js_type.Jstm_switch($3, $5)}
  ;

  case_block:
    LCBRACE list(case_clause) RCBRACE {$2}
        |LCBRACE list(case_clause) default_clause list(case_clause) RCBRACE {$2 @ [$3] @ $4}
  ;

  case_clause:
    KEYWORD_CASE expression COLON list(statement) {Js_type.Jcas_case(Some($2), $4)}
  ;

  default_clause:
    KEYWORD_DEFAULT COLON list(statement) {Js_type.Jcas_case(None, $3)}
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
    KEYWORD_DEBUGGER SEMICOLON {Js_type.Jstm_debugger}
  ;

  (* --- Statement grammers *)

  (* Programs and functions grammer *)

  function_common:
    KEYWORD_FUNCTION id=identifier LPAREN ls=separated_list(COMMA, formal_parameter) RPAREN LCBRACE body=function_body RCBRACE {
      (id, ls, Js_type.Jstm_block(body))
    }
  ;

  function_declaration:
    common=function_common {
      let id, ls ,body = common in
      Js_type.Jdec_function (id, ls, body)
    }
  ;

  function_expression:
    common=function_common {
      let id, ls ,body = common in
      Js_type.Jexp_function (Some(id), ls, body)
    }
                                                                                                         |KEYWORD_FUNCTION LPAREN ls=separated_list(COMMA, formal_parameter) RPAREN LCBRACE body=function_body RCBRACE {
      Js_type.Jexp_function (None, ls, Js_type.Jstm_block(body))
    }
  ;

  formal_parameter:
    identifier {$1}
  ;

  function_body:
    {[]}
  ;

  source_element:
    statement {$1}
           |function_declaration {Js_type.Jstm_function($1)}
           |comment {$1}
  ;

  program:
    function_body {Js_type.Jprog_program ($1)}
  ;

  (* --- Programs and functions grammer *)

  identifier:
    IDENT {Js_type.Jexp_ident($1)}
  ;

  identifier_name:
    identifier_start {$1}
           |identifier_name identifier_part {$1 ^ $2}
  ;

  identifier_start:
    CHAR          { Char.escaped($1) }
           | UNDERSCORE   { "_" }
           | DOLLAR   { "$" }
  ;

  identifier_part:
    identifier_start {$1}
           | DIGIT  { $1 }
  ;

  literal:
    NULL {Js_type.Jl_null}
           |TRUE {Js_type.Jl_bool(true)}
           |FALSE {Js_type.Jl_bool(false)}
           |string {$1}
           |number {$1}
  ;

  string:
    DOUBLE_QUOTE DOUBLE_QUOTE  { Js_type.Jl_string("\"\"", "") }
           | SINGLE_QUOTE SINGLE_QUOTE  { Js_type.Jl_string("''", "") }
           | DOUBLE_QUOTE double_chars DOUBLE_QUOTE  { Js_type.Jl_string(Printf.sprintf "\"%s\"" $2, $2) }
           | SINGLE_QUOTE single_chars SINGLE_QUOTE  { Js_type.Jl_string(Printf.sprintf "'%s'" $2, $2 )}
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
    integer       { Js_type.Jl_number($1, $1) }
           | integer frac  { Js_type.Jl_number($1 ^ $2, $1 ^ $2) }
           | integer exp   { Js_type.Jl_number($1 ^ $2, $1 ^ $2) }
           | integer frac exp { Js_type.Jl_number($1 ^ $2 ^ $3, $1 ^ $2 ^ $3) }
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

  tok_minus: MINUS {"-"};
  tok_plus: PLUS {"+"};
  tok_less: LESS {"<"};
  tok_greater: GREATER {">"};
  tok_less_than: LESS_THAN {"<="};
  tok_greater_than: GREATER_THAN {">="};
  tok_equal: EQUAL {"=="};
  tok_not_equal: NOT_EQUAL {"!="};
  tok_deep_equal: DEEP_EQUAL {"==="};
  tok_deep_not_equal: DEEP_NOT_EQUAL {"!=="};
  tok_multi: MULTI {"*"};
  tok_mod: MOD {"%"};
  tok_increment: INCREMENT {"++"};
  tok_decrement: DECREMENT {"--"};
  tok_lshift: LSHIFT {"<<"};
  tok_rshift: RSHIFT {">>"};
  tok_and: AND {"&"};
  tok_or: OR {"|"};
  tok_xor: XOR {"^"};
  tok_not: NOT {"!"};
  tok_comp: COMP {"~"};
  tok_logical_and: LOGICAL_AND {"&&"};
  tok_logical_or: LOGICAL_OR {"||"};
  tok_assign: ASSIGN {"="};
  tok_plus_assign: PLUS_ASSIGN {"+="};
  tok_minus_assign: MINUS_ASSIGN {"-="};
  tok_multi_assign: MULTI_ASSIGN {"*="};
  tok_mod_assign: MOD_ASSIGN {"%="};
  tok_lshift_assign: LSHIFT_ASSIGN {"<<="};
  tok_rshift_assign: RSHIFT_ASSIGN {">>="};
  tok_and_assign: AND_ASSIGN {"&="};
  tok_or_assign: OR_ASSIGN {"|="};
  tok_xor_assign: XOR_ASSIGN {"^="};
  tok_div: DIV {"/"};

  keyword_delete: KEYWORD_DELETE {"delete"};
  keyword_void: KEYWORD_VOID {"void"};
  keyword_in: KEYWORD_IN {"in"};
  keyword_instanceof: KEYWORD_INSTANCEOF {"instanceof"};
  keyword_typeof: KEYWORD_TYPEOF {"typeof"};

  comment:
    MULTI_LINE_COMMENT {Js_type.Jstm_comment_block ($1)}
           |SINGLE_LINE_COMMENT {Js_type.Jstm_comment_line ($1)}
  ;
