(* t ::= true
        false
        if t then t else t 0
        succ t
        pred t
        iszero t *)
type term =
  | TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

let rec isnumericval t =
  match t with
  | TmZero -> true
  | TmSucc t1 -> isnumericval t1
  | _ -> false

let rec isval t =
  match t with
  | TmTrue | TmFalse -> true
  | t when isnumericval t -> true
  | _ -> false

let term_2 = TmSucc (TmSucc TmZero)

let () =
  let result = isval term_2 in
    Printf.printf "Is 2 a value? %b\n" result
