type term =
  | TmVar  int * int
  | TmAbs  string * term
  | TmApp  term * terms


let rec printtm ctx t = match t with 
  | TmAbs(fi,x,t1) ->
    let (ctx', x') = pickfreshname ctx x in
    pr "(lambda "; pr x'; pr ". "; printtm ctx t2; pr ")"
  | TmApp(fi, t1, t2) ->
    pr "("; printtm ctx t1; pr " "; printtm ctx t2; pr ")"
  | TmVar(fi, t1, t2) ->
    if ctxlength ctx = n then
      pr (index2name fi ctx x)
    else
      pr "[bad index]"

type context = (string * bindind) list

type binding = NameBind

let termShift d t =
  let rec walk c t = match t with
    | TmVar(fi, x, n) -> if x>=c then TmVar(fi, x+d, n+d) else TmVar(fi, x, n+d)
    | TmAbs(fi, x, t1) -> TmAbs(fi, x, walk (c+1) t1)
    | TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2) in walk 0 t

let termSubst j s t =
  let rec walk c t = match t with
    | TmVar(fi, x, n) -> if x=j+c then termShift c s else TmVar(fi, x, n)
    | TmAbs(fi, x, t1) -> TmAbs(fi, x, walk (c+1) t1)
    | TmApp(fi, t1, t2) -> TmApp(fi, walk c t1, walk c t2) in walk 0 t

let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let rec isval ctx t = match t with TmAbs(_,_,_) -> true
  | _ -> false


let rec eval1 ctx t = match t with 
  | TmApp(fi, TmAbs(_, x, t12), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2) 
  | _ ->
    raise NoRuleApplies


  
let rec eval ctx t =
  try let t' = eval1 ctx t
    in eval ctx t' 
  with NoRuleApplies -> t



  type context = (string * binding) list