open Oqc

let (!!) f =
  fun v ->
  let r = f v in
  match r.status with
  | Pass -> { r with status = Fail; expect = Unexpected }
  | Fail -> { r with status = Pass }
  | Undefined -> r

let (&&&) f1 f2 =
  fun v ->
  let r = f1 v in
  match r.status with
  | Pass -> f2 v
  | Fail | Undefined -> r

let (|||) f1 f2 =
  fun v ->
  let r = f1 v in
  match r.status with
  | Pass -> r
  | Fail | Undefined -> f2 v

let (==>) f1 f2 =
  fun v ->
  let r = f1 v in
  match r.status with
  | Pass -> f2 v
  | Fail | Undefined -> { r with status = Undefined }

let (<==>) f1 f2 =
  fun v ->
  let r1 = f1 v and r2 = f2 v in
  if r1.status <> r2.status then
    { status = Fail; expect = Unexpected;
      details = "not equivalent" }
  else if r1.status = Undefined then
    { status = Undefined; expect = Unexpected;
      details = "undefined equivalence" }
  else
    { status = Pass; expect = Expected; details = "ok" }

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
