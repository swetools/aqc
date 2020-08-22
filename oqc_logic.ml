open Oqc

let (!!) f =
  fun v ->
        match f v with
        | Pass -> Fail "unexpected success"
        | Fail msg -> XFail msg
        | Skip msg -> Skip msg
        | XFail msg -> Fail msg

let (&&&) f1 f2 =
  fun v ->
  match f1 v with
  | Pass | XFail _ -> f2 v
  | Skip msg -> Skip msg
  | Fail msg -> Fail msg

let (|||) f1 f2 =
  fun v ->
  match f1 v with
  | Skip _ | Fail _ -> f2 v
  | Pass -> Pass
  | XFail msg -> XFail msg

let (==>) f1 f2 =
  fun v ->
  match f1 v with
  | Fail msg | Skip msg -> Skip msg
  | Pass | XFail _ -> f2 v

let parens s = "(" ^ s ^ ")"

module NotProp (P : PROPOSITION) =
  struct
    let check =  !! P.check

    let description = parens ("¬" ^ P.description)
  end

module AndProp (P1 : PROPOSITION) (P2 : PROPOSITION) =
  struct
    let check = P1.check &&& P2.check

    let description = parens (P1.description ^ " ∧ " ^ P2.description)
  end

module OrProp (P1 : PROPOSITION) (P2 : PROPOSITION) =
  struct
    let check = P1.check ||| P2.check

    let description = parens (P1.description ^ " ∨ " ^ P2.description)
  end

module CondProp (P1 : PROPOSITION) (P2 : PROPOSITION) =
  struct
    let check = P1.check ==> P2.check

    let description = parens (P1.description ^ " → " ^ P2.description)
  end

module NotPred (P : PREDICATE) =
  struct
    module D = P.D

    let predicate = !! P.predicate

    let description arg = parens ("¬" ^ P.description arg)
  end

module AndPred (P1 : PREDICATE) (P2 : PREDICATE with type D.t = P1.D.t) =
  struct
    module D = P1.D

    let predicate = P1.predicate &&& P2.predicate

    let description arg = parens (P1.description arg ^ " ∧ " ^ P2.description arg)
  end

module OrPred (P1 : PREDICATE) (P2 : PREDICATE with type D.t = P1.D.t) =
  struct
    module D = P1.D

    let predicate = P1.predicate ||| P2.predicate

    let description arg = parens (P1.description arg ^ " ∨ " ^ P2.description arg)
  end


module CondPred (P1 : PREDICATE) (P2 : PREDICATE with type D.t = P1.D.t) =
  struct
    module D = P1.D

    let predicate = P1.predicate ==> P2.predicate

    let description arg = parens (P1.description arg ^ " → " ^ P2.description arg)
  end

let uncurry f = fun (x, y) -> f x y
let curry f = fun x y -> f (x, y)

module NotRel (R : RELATION) =
  struct
    module D1 = R.D1
    module D2 = R.D2

    let relation = curry (!! (uncurry R.relation))

    let description arg1 arg2 = parens ("¬" ^ R.description arg1 arg2)
  end


module AndRel (R1 : RELATION) (R2 : RELATION with type D1.t = R1.D1.t and type D2.t = R1.D2.t) =
  struct
    module D1 = R1.D1
    module D2 = R1.D2

    let relation = curry (uncurry R1.relation &&& uncurry R2.relation)

    let description arg1 arg2 = parens (R1.description arg1 arg2 ^ " ∧ " ^ R2.description arg1 arg2)
  end

module OrRel (R1 : RELATION) (R2 : RELATION with type D1.t = R1.D1.t and type D2.t = R1.D2.t) =
  struct
    module D1 = R1.D1
    module D2 = R1.D2

    let relation = curry (uncurry R1.relation ||| uncurry R2.relation)

    let description arg1 arg2 = parens (R1.description arg1 arg2 ^ " ∨ " ^ R2.description arg1 arg2)
  end

module CondRel (R1 : RELATION) (R2 : RELATION with type D1.t = R1.D1.t and type D2.t = R1.D2.t) =
  struct
    module D1 = R1.D1
    module D2 = R1.D2

    let relation = curry (uncurry R1.relation ==> uncurry R2.relation)

    let description arg1 arg2 = parens (R1.description arg1 arg2 ^ " → " ^ R2.description arg1 arg2)
  end
