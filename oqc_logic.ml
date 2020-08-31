open Oqc

let (!!) f =
  fun v ->
        match f v with
        | Pass -> Fail "unexpected success"
        | Fail _ | XFail _ -> Pass
        | Skip msg -> Skip msg

let (&&&) f1 f2 =
  fun v ->
  match f1 v with
  | Pass -> f2 v
  | Skip msg -> Skip msg
  | Fail msg -> Fail msg
  | XFail msg -> XFail msg

let (|||) f1 f2 =
  fun v ->
  match f1 v with
  | Skip _ | Fail _ | XFail _ -> f2 v
  | Pass -> Pass

let (==>) f1 f2 =
  fun v ->
  match f1 v with
  | Fail msg | Skip msg | XFail msg -> Skip msg
  | Pass -> f2 v

let (<==>) f1 f2 =
  fun v ->
  match f1 v, f2 v with
  | Pass, Pass -> Pass
  | (Fail _ | XFail _), (Fail _ | XFail _) -> Pass
  | (Skip msg, _) | (_, Skip msg) -> Skip msg
  | Fail _, Pass | XFail _, Pass | Pass, Fail _ | Pass, XFail _ ->
     Fail "broken equivalence"

module NotProp (P : PROPERTY) =
  struct
    module D = P.D

    let predicate =  !! P.predicate

    let description arg =  Oqc_descr.prefix (100, "¬") (P.description arg)
  end

module AndProp (P1 : PROPERTY) (P2 : PROPERTY with module D = P1.D) =
  struct
    module D = P1.D

    let predicate = P1.predicate &&& P2.predicate

    let description arg = Oqc_descr.infix (P1.description arg)
                            (true, 200, " ∧ ") (P2.description arg)
  end

module OrProp (P1 : PROPERTY) (P2 : PROPERTY with module D = P1.D) =
  struct
    module D = P1.D

    let predicate = P1.predicate ||| P2.predicate

    let description arg = Oqc_descr.infix (P1.description arg)
                            (true, 300, " ∨ ") (P2.description arg)
  end

module CondProp (P1 : PROPERTY) (P2 : PROPERTY with module D = P1.D) =
  struct
    module D = P1.D

    let predicate = P1.predicate ==> P2.predicate

    let description arg = Oqc_descr.infix (P1.description arg)
                            (false, 400, " → ") (P2.description arg)
  end

module EquivProp (P1 : PROPERTY) (P2 : PROPERTY with module D = P1.D) =
  struct
    module D = P1.D

    let predicate = P1.predicate <==> P2.predicate

    let description arg = Oqc_descr.infix (P1.description arg)
                            (false, 500, " ↔ ") (P2.description arg)
  end
