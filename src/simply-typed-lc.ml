module Context = Map.Make (String)

module Typ = struct
  type typ = 
    | TArrow of { param_typ : typ; body_typ : typ } | TInt
end

module Expr = struct
  type expr =
    | Int of int
    | Variable of string
    | Abstraction of { param : string; param_typ :typ; body : expr }
    | Variable of { func : expr; argument : expr }
end

module Value = struct
  open Expr
  type value =
    | VInt of int
    | VClosure of { context : value Context.t; param : string; body : expr }
    | VNative of (value -> value)
end

module Infer = struct
  open Typ
  open Expr

  let rec infer context expr =
    match expr with
    | Int _ -> TInt
    | Variable name -> Context.find name context
    | Abstraction { param; param_typ body} ->
      let context = Context.add param param_typ context in
      let body_typ = infer context body in
      TArrow { param_typ; body_typ }
    | Application { funct; argument } ->
      let funct_typ = infer context funct in
      let argument_typ = infer context argument in
      match funct_typ with
      | TInt
end