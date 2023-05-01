open Ast

type typ =
  | TInt
  | TBool

module Env : sig
  type t

  val empty : t

  val lookup : t -> string -> typ

  val extend : t -> string -> typ -> t
end = struct
  type t = (string * typ) list

  let empty = []

  let lookup env x =
    try List.assoc x env
    with Not_found -> failwith "Unbound variable"

  let extend env x ty =
    (x, ty) :: env
end

let rec typeof env = function
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x -> Env.lookup env x
  | Let (x, e1, e2) -> typeof_let env x e1 e2
  | Binop (bop, e1, e2) -> typeof_bop env bop e1 e2
  | If (e1, e2, e3) -> typeof_if env e1 e2 e3

and typeof_let env x e1 e2 =
  let t1 = typeof env e1 in
  let env' = Env.extend env x t1 in
  typeof env' e2

and typeof_bop env bop  e1 e2 =
  let t1, t2 = typeof env e1, typeof env e2 in
  match bop, t1, t2 with
  | Add, TInt, TInt | Mult, TInt, TInt -> TInt
  | Leq, TInt, TInt -> TBool
  | _ -> failwith "Operator and operand type mismatch"

and typeof_if env e1 e2 e3 =
  if typeof env e1 = TBool
  then begin
    let t2 = typeof env e2 in
    if t2 = typeof env e3 then t2
    else failwith "Branches of if must have same type"
  end
  else failwith "Guard of if must be boolean"
