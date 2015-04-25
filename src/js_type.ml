open CamomileLibrary

(* Literals *)
type literal =
  | Jl_null
  | Jl_bool of bool
  | Jl_string of UTF8.t * UTF8.t
  | Jl_number of UTF8.t * UTF8.t

(* Expressions *)
and expression =
    Jexp_ident of UTF8.t
  | Jexp_this
  | Jexp_sequence of expression list
  | Jexp_literal of literal
  | Jexp_array of expression list
  | Jexp_object of expression list
  | Jexp_property of expression * expression
  | Jexp_member of expression * expression
  | Jexp_new of expression * expression list
  | Jexp_call of expression * expression list
  | Jexp_unary of expression * UTF8.t
  | Jexp_binary of expression * UTF8.t * expression
  | Jexp_update of expression * UTF8.t * bool
  | Jexp_conditional of expression * expression * expression
  | Jexp_assignment of expression * UTF8.t * expression
  | Jexp_function of expression option * expression list * statement
and declaration =
    Jdec_var of expression * expression option
  | Jdec_function of expression * expression list * statement
and program = Jprog_program of statement list
and statement =
    Jstm_var of declaration list
  | Jstm_empty
  | Jstm_if of expression * statement * statement option
  | Jstm_expression of expression
  | Jstm_do_while of expression * statement
  | Jstm_while of expression * statement
  | Jstm_for of expression option * expression option * expression option * statement
  | Jstm_for_dec of declaration list * expression option * expression option * statement
  | Jstm_for_in of expression * expression * statement
  | Jstm_for_in_dec of declaration * expression * statement
  | Jstm_block of statement list
  | Jstm_continue of expression option
  | Jstm_break of expression option
  | Jstm_return of expression option
  | Jstm_with of expression * statement
  | Jstm_switch of expression * case list
  | Jstm_labelled of expression * statement
  | Jstm_try of statement * catch option * finally option
  | Jstm_throw of expression
  | Jstm_debugger
  | Jstm_function of declaration
  | Jstm_comment_block of UTF8.t
  | Jstm_comment_line of UTF8.t

and catch = Jstm_catch of expression * statement
and finally = Jstm_finally of statement
and case = Jcas_case of expression option * statement list
