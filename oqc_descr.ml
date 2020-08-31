type t = Atom of string
       | Variable of string
       | Prefix of int * string * t
       | Suffix of int * string * t
       | Infix of bool * int * string * t * t
       | Circumfix of string * string * t

let precedence x =
  match x with
  | Atom _ | Circumfix _ | Variable _ -> 0
  | Prefix (p, _, _) | Suffix (p, _, _) | Infix (_, p, _, _, _) -> p

let is_associative x =
  match x with
  | Atom _ | Circumfix _ | Prefix _  | Suffix _ | Variable _ -> true
  | Infix (assoc, _, _, _, _) -> assoc

let rec to_string x =
  let parens assoc p v =
    let str = to_string v and
        p0 = precedence v and
        assoc0 = is_associative v in
    if p0 > p ||
         (p0 == p && (not assoc || not assoc0)) then
      "(" ^ str ^ ")"
    else str
  in
  match x with
  | Atom str | Variable str -> str
  | Prefix (p, op, arg) -> op ^ parens true p arg
  | Suffix (p, op, arg) -> parens true p arg ^ op
  | Circumfix (pfx, sfx, arg) -> pfx ^ to_string arg ^ sfx
  | Infix (assoc, p, op, arg1, arg2) -> parens assoc p arg1 ^ op ^
                                          parens assoc p arg2

let atom str = Atom str

let variable str = Variable str

let prefix (p, op) v = Prefix (p, op, v)

let suffix v (p, op) = Suffix (p, op, v)

let wrap pfx v sfx = Circumfix (pfx, sfx, v)

let infix arg1 (assoc, p, op) arg2 = Infix (assoc, p, op, arg1, arg2)

let rec reinfix (assoc, p, op) x =
  match x with
  | Atom _ | Variable _ -> raise (Failure "cannot insert an infix op")
  | Prefix (_, _, y) | Suffix (_, _, y) | Circumfix (_, _, y) ->
     reinfix (assoc, p, op) y
  | Infix (_, _, _, x, y) -> Infix (assoc, p, op, x, y)

let variables x =
  let rec collect x vars =
    match x with
    | Atom _ -> vars
    | Variable v when List.mem v vars -> vars
    | Variable v -> v :: vars
    | Prefix (_, _, arg) | Suffix (_, _, arg) | Circumfix (_, _, arg) ->
       collect arg vars
    | Infix (_, _, _, arg1, arg2) ->
       collect arg2 (collect arg1 vars)
  in
  collect x []
