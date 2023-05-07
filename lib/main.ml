open Ast
open Interp
open Typecheck

let typecheck (e : expr) : expr =
  ignore (typeof Env.empty e); e

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let eval (e : expr) : expr =
  if is_value e then e
  else e |> step |> typecheck |> eval_big

let interp (s : string) : expr =
  let ast = parse s in
  eval ast
