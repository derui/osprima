open Core.Std

module J = Tiny_json.Json
module T = Js_type

let ast typ params = J.Object (("type", J.String typ) :: params)

let or_else f = function
  | None -> J.Null
  | Some v -> f v

let ignore_null = function
  | J.Null -> false
  | _ -> true

let literal v lit = ast "Literal" [("value", v);("raw", J.String lit)]
let regexp regex flag =
  let buf = Buffer.create 1 in
  Buffer.add_string buf "/";
  Buffer.add_string buf regex;
  Buffer.add_string buf "/";
  Buffer.add_string buf flag;
  let value = Buffer.contents buf in
  ast "Literal" [("value", J.String (value));
                 ("raw", J.String (value));
                 ("regex", J.Object ([("pattern", J.String regex);
                                      ("flags", J.String flag)]))
                ]

let hex_to_int = function
  | '0'..'9' as c -> (Char.to_int c) - (Char.to_int '0')
  | 'a' | 'A' -> 10
  | 'b' | 'B' -> 11
  | 'c' | 'C' -> 12
  | 'd' | 'D' -> 13
  | 'e' | 'E' -> 14
  | 'f' | 'F' -> 15
  | c -> failwith ("Unknown Hex charactor: " ^ (String.of_char c))

let replace_over_espaced_sequences s =
  let buf = String.length s |> Buffer.create
  and subbuf = Buffer.create 0 in

  String.iter s ~f:(fun c ->
    match c with
    | '\\' when Buffer.length subbuf = 0 -> Buffer.add_char subbuf c
    | _ when Buffer.length subbuf <> 0 -> begin
      match c with
      | 'b' -> Buffer.add_bytes buf "\b"
      | 'n' -> Buffer.add_bytes buf "\n"
      | 'r' -> Buffer.add_bytes buf "\r"
      | 't' -> Buffer.add_bytes buf "\t"
      | '\012' -> Buffer.add_bytes buf "\012" (* \f *)
      | '\011' -> Buffer.add_bytes buf "\011" (* \v *)
      | '\\' -> Buffer.add_bytes buf "\\"
      | '"' -> Buffer.add_bytes buf "\""
      | '\'' -> Buffer.add_bytes buf "'"
      | _ -> Buffer.add_char subbuf c; Buffer.add_buffer buf subbuf
    end;
      Buffer.reset subbuf
    | _ -> Buffer.add_char buf c
  );

  Buffer.contents buf

(* converters *)
let rec literal_to_json = function
  | T.Jl_null -> literal J.Null "null"
  | T.Jl_bool b -> literal (J.Bool b) (string_of_bool b)
  | T.Jl_string (raw, s) -> literal (J.String (replace_over_espaced_sequences s)) raw
  | T.Jl_number (raw, n) -> literal (J.Number n) raw
  | T.Jl_hex_digit (raw) ->
     let hex = String.sub raw 2 (String.length raw - 2) |> String.to_list |> List.rev in
     let num = List.foldi ~init:0 ~f:(fun index num c ->
       let i = hex_to_int c in
       let i = i lsl (index * 4) in
       num + i
     ) hex in
     literal (J.Number (string_of_int num)) raw
  | T.Jl_regex (regex, flags) -> regexp regex flags

and exp_to_json = function
  | T.Jexp_ident ident -> ast "Identifier" [("name", J.String ident)]
  | T.Jexp_this -> ast "ThisExpression" []
  | T.Jexp_sequence sequence -> begin
    match sequence with
    | [] -> J.Null
    | [exp] -> exp_to_json exp
    | _ -> ast "SequenceExpression" [("expressions", J.Array (List.map ~f:exp_to_json sequence))]
  end
  | T.Jexp_literal lit -> literal_to_json lit
  | T.Jexp_array ary -> ast "ArrayExpression" [("elements", J.Array (List.map ~f:exp_to_json ary))]
  | T.Jexp_object pairs -> ast "ObjectExpression" [
    ("properties", J.Array(List.map ~f:exp_to_json pairs))]
  | T.Jexp_property (key, v) -> ast "Property" [("key", exp_to_json key);
                                                ("computed", J.Bool false);
                                                ("value", exp_to_json v);
                                                ("kind", J.String "init");
                                                ("method", J.Bool false);
                                                ("shorthand", J.Bool false)]
  | T.Jexp_member (callee, memb, comp) -> ast "MemberExpression" [("computed", J.Bool comp);
                                                                  ("object", exp_to_json callee);
                                                                  ("property", exp_to_json memb)]
  | T.Jexp_new (callee, args) -> ast "NewExpression" [
    ("callee", exp_to_json callee); ("arguments", J.Array(List.map ~f:exp_to_json args))]
  | T.Jexp_call (callee, args) -> ast "CallExpression" [
    ("callee", exp_to_json callee); ("arguments", J.Array(List.map ~f:exp_to_json args))]
  | T.Jexp_unary (exp, op) -> ast "UnaryExpression" [("operator", J.String op);
                                                     ("argument", exp_to_json exp);
                                                     ("prefix", J.Bool true)]
  | T.Jexp_binary (left, op, right) -> ast "BinaryExpression" [("operator", J.String op);
                                                               ("left", exp_to_json left);
                                                               ("right", exp_to_json right)]
  | T.Jexp_logical (left, op, right) -> ast "LogicalExpression" [("operator", J.String op);
                                                                 ("left", exp_to_json left);
                                                                 ("right", exp_to_json right)]
  | T.Jexp_update (exp, op, prefix) -> ast "UpdateExpression" [("operator", J.String op);
                                                               ("argument", exp_to_json exp);
                                                               ("prefix", J.Bool prefix)]
  | T.Jexp_conditional (test, cons, alt) ->
     ast "ConditionalExpression" [("test", exp_to_json test);("consequent", exp_to_json cons);
                                  ("alternate", exp_to_json alt)]
  | T.Jexp_assignment (left, op, right) ->
     ast "AssignmentExpression" [("operator", J.String op);
                                 ("left", exp_to_json left);
                                 ("right", exp_to_json right)]
  | T.Jexp_function (name, args, state) ->
     ast "FunctionExpression" [
       ("id", match name with | None -> J.Null | Some name -> exp_to_json name);
       ("params", J.Array (List.map ~f:exp_to_json args));
       ("defaults", J.Array []);
       ("body", statement_to_json state);
       ("generator", J.Bool false);
       ("expression", J.Bool false)]

and dec_to_json = function
  | T.Jdec_var (e, init) ->
     ast "VariableDeclarator" [("id", exp_to_json e);
                               ("init", or_else exp_to_json init)]
  | T.Jdec_function (e, args, stmt) ->
     ast "FunctionDeclaration" [("id", exp_to_json e);
                                ("params", J.Array (List.map ~f:exp_to_json args));
                                ("defaults", J.Array []);
                                ("body", statement_to_json stmt);
                                ("generator", J.Bool false);
                                ("expression", J.Bool false)]

(* Statement module to serialize abstract syntax tree *)

and statement_to_json = function
  | T.Jstm_var decs -> ast "VariableDeclaration" [("declarations", J.Array (List.map ~f:dec_to_json decs));
                                                  ("kind", J.String "var")]
  | T.Jstm_empty -> ast "EmptyStatement" []
  | T.Jstm_if (cond, stmt, alternate) ->
     let alternate = match alternate with
       | None -> J.Null
       | Some s -> statement_to_json s in
     ast "IfStatement" [("test", exp_to_json cond);
                        ("consequent", statement_to_json stmt);
                        ("alternate", alternate)]
  | T.Jstm_expression e -> ast "ExpressionStatement" [("expression", exp_to_json e)]
  | T.Jstm_do_while (cond, stmt) ->
     ast "DoWhileStatement" [("body", statement_to_json stmt);
                             ("test", exp_to_json cond)]
  | T.Jstm_while (cond, stmt) ->
     ast "WhileStatement" [("test", exp_to_json cond);
                           ("body", statement_to_json stmt)]
  | T.Jstm_for (init, test, update, stmt) ->
     ast "ForStatement" [("init", or_else exp_to_json init);
                         ("test", or_else exp_to_json test);
                         ("update", or_else exp_to_json update);
                         ("body", statement_to_json stmt)]
  | T.Jstm_for_dec (init, test, update, stmt) ->
     ast "ForStatement" [("init", statement_to_json (T.Jstm_var init));
                         ("test", or_else exp_to_json test);
                         ("update", or_else exp_to_json update);
                         ("body", statement_to_json stmt)]
  | T.Jstm_for_in (left, right, stmt) ->
     ast "ForInStatement" [("left", exp_to_json left);
                           ("right", exp_to_json right);
                           ("body", statement_to_json stmt);
                           ("each", J.Bool false)]
  | T.Jstm_for_in_dec (left, right, stmt) ->
     ast "ForInStatement" [("left", statement_to_json (T.Jstm_var [left]));
                           ("right", exp_to_json right);
                           ("body", statement_to_json stmt);
                           ("each", J.Bool false)]
  | T.Jstm_block stmt ->
     ast "BlockStatement" [("body", J.Array(List.map ~f:statement_to_json stmt |> List.filter ~f:ignore_null))]
  | T.Jstm_continue e ->
     ast "ContinueStatement" [("label", or_else exp_to_json e)]
  | T.Jstm_break e ->
     ast "BreakStatement" [("label", or_else exp_to_json e)]
  | T.Jstm_return e ->
     ast "ReturnStatement" [("argument", or_else exp_to_json e)]
  | T.Jstm_with (obj, stmt) ->
     ast "WithStatement" [("object", exp_to_json obj);
                          ("body", statement_to_json stmt)]
  | T.Jstm_switch (cond, cases) ->
     ast "SwitchStatement" [("discriminant", exp_to_json cond);
                            ("cases", J.Array(List.map ~f:case_to_json cases))]
  | T.Jstm_labelled (e, stmt) ->
     ast "LabelledStatement" [("label", exp_to_json e);
                              ("body", statement_to_json stmt)]
  | T.Jstm_try (block, handler, finalizer) ->
     let handler = or_else catch_to_json handler in
     ast "TryStatement" [("block", statement_to_json block);
                         ("handlers", match handler with | J.Null -> J.Array []
                         | _ -> J.Array [handler]);
                         ("handler", handler);
                         ("guardedHandlers", J.Array []);
                         ("finalizer", or_else finalizer_to_json finalizer)]
  | T.Jstm_throw e ->
     ast "ThrowStatement" [("argument", exp_to_json e)]
  | T.Jstm_debugger -> ast "DebuggerStatement" []
  | T.Jstm_function dec -> dec_to_json dec
  | T.Jstm_comment_block _ -> J.Null
  | T.Jstm_comment_line _ -> J.Null
and case_to_json = function
  | T.Jcas_case (e, stmt) -> ast "SwitchCase" [("test", or_else exp_to_json e);
                                               ("consequent", J.Array(List.map ~f:statement_to_json stmt |> List.filter ~f:ignore_null))]
and catch_to_json = function
  | T.Jstm_catch (e, stmt) -> ast "CatchClause" [("param", exp_to_json e);
                                                 ("body", statement_to_json stmt)]
and finalizer_to_json = function
  | T.Jstm_finally stmt -> statement_to_json stmt

let program stmt = J.Object [("type", J.String "Program"); ("body", stmt)]

let program_to_json = function
  | T.Jprog_program stmt -> begin
    let stmt = List.map ~f:statement_to_json stmt |> List.filter ~f:ignore_null in
    program (J.Array stmt)
  end
