open Ast
open Interp

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec eval_small (e : expr) : expr =
  if is_value e then e
  else e |> step |> eval_small

let eval (e : expr) : expr =
  if is_value e then e
  else e |> step |> eval_big

let interp (s : string) : expr =
  let ast = parse s in
  eval ast
