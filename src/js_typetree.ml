open Core.Std

module J = Tiny_json.Json
module T = Js_type

let ast typ params = J.Object (("type", J.String typ) :: params)

let or_else f = function
  | None -> J.Null
  | Some v -> f v


module Literal = struct
  type t = T.literal

  let literal v lit = ast "Literal" [("value", v);("raw", J.String lit)]

  let to_json = function
    | T.Jl_null -> literal J.Null "null"
    | T.Jl_bool b -> literal (J.Bool b) (string_of_bool b)
    | T.Jl_string (raw, s) -> literal (J.String s) raw
    | T.Jl_number (raw, n) -> literal (J.Number n) raw
end

module Expression = struct
  type t = T.expression

  let rec to_json = function
    | _ -> failwith "not implemented expression"
end

module Declaration = struct
  type t = T.declaration

  let to_json = function
    | T.Jdec_var (e, init) ->
       ast "VariableDeclarator" [("id", Expression.to_json e);
                                 ("init", or_else Expression.to_json init)]
    | _ -> failwith "not implemented declaration"
end

module Cases = struct
end

(* Statement module to serialize abstract syntax tree *)
module Statement = struct

  type t = T.statement

  let rec to_json = function
    | T.Jstm_var decs -> ast "VariableDeclaration" [("declarations", J.Array (List.map ~f:Declaration.to_json decs))]
    | T.Jstm_empty -> ast "EmptyStatement" []
    | T.Jstm_if (cond, stmt, els) ->
       ast "IfStatement" [("test", Expression.to_json cond);
                          ("consequent", to_json stmt);
                          ("alternate", match els with
                          | None -> J.Null
                          | Some els -> to_json stmt)]
    | T.Jstm_expression e -> ast "ExpressionStatement" [("expression", Expression.to_json e)]
    | T.Jstm_do_while (cond, stmt) ->
       ast "DoWhileStatement" [("body", to_json stmt)]
    | T.Jstm_while (cond, stmt) ->
       ast "WhileStatement" [("body", to_json stmt);
                             ("test", Expression.to_json cond)]
    | T.Jstm_for (init, test, update, stmt) ->
       ast "ForStatement" [("init", or_else Expression.to_json init);
                           ("test", or_else Expression.to_json test);
                           ("update", or_else Expression.to_json update);
                           ("body", to_json stmt)]
    | T.Jstm_for_dec (init, test, update, stmt) ->
       ast "ForStatement" [("init", to_json (T.Jstm_var init));
                           ("test", or_else Expression.to_json test);
                           ("update", or_else Expression.to_json update);
                           ("body", to_json stmt)]
    | T.Jstm_for_in (left, right, stmt) ->
       ast "ForInStatement" [("left", Expression.to_json left);
                             ("right", Expression.to_json right);
                             ("body", to_json stmt);
                             ("each", J.Bool false)]
    | T.Jstm_for_in_dec (left, right, stmt) ->
       ast "ForInStatement" [("left", to_json (T.Jstm_var [left]));
                             ("right", Expression.to_json right);
                             ("body", to_json stmt);
                             ("each", J.Bool false)]
    | T.Jstm_block stmt ->
       ast "BlockStatement" [("body", J.Array(List.map ~f:to_json stmt))]
    | T.Jstm_continue e ->
       ast "ContinueStatement" [("label", or_else Expression.to_json e)]
    | T.Jstm_break e ->
       ast "BreakStatement" [("label", or_else Expression.to_json e)]
    | T.Jstm_return e ->
       ast "ReturnStatement" [("argument", or_else Expression.to_json e)]
    | T.Jstm_with (obj, stmt) ->
       ast "WithStatement" [("object", Expression.to_json obj);
                            ("body", to_json stmt)]
    | T.Jstm_switch (cond, cases) ->
       ast "SwitchStatement" [("discriminant", Expression.to_json cond);
                              ("cases", J.Array(List.map ~f:case_to_json cases))]
    | T.Jstm_labelled (e, stmt) ->
       ast "LabelledStatement" [("label", Expression.to_json e);
                                ("body", to_json stmt)]
    | T.Jstm_try (block, handler, finalizer) ->
       let handler = or_else catch_to_json handler in
       ast "TryStatement" [("block", to_json block);
                           ("handlers", match handler with | J.Null -> J.Array []
                           | _ -> J.Array [handler]);
                           ("handler", handler);
                           ("finalizer", or_else finalizer_to_json finalizer)]
    | T.Jstm_throw e ->
       ast "ThrowStatement" [("argument", Expression.to_json e)]
    | T.Jstm_debugger -> ast "DebuggerStatement" []
    | T.Jstm_function dec -> Declaration.to_json dec
  and case_to_json = function
    | T.Jcas_case (e, stmt) -> ast "SwitchCase" [("test", or_else Expression.to_json e);
                                               ("consequent", J.Array(List.map ~f:to_json stmt))]
  and catch_to_json = function
    | T.Jstm_catch (e, stmt) -> ast "CatchClause" [("param", Expression.to_json e);
                                                 ("body", to_json stmt)]
  and finalizer_to_json = function
    | T.Jstm_finally stmt -> to_json stmt
end

module Program = struct
  type t = T.program

  let program stmt = J.Object [("type", J.String "Program"); ("body", stmt)]

  let to_json = function
    | [] -> program (J.Array [])
    | stmt -> program (J.Array (List.map ~f:Statement.to_json stmt))
end
