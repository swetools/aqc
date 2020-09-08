type expectation = Expected
                 | Unexpected

type status = Pass
            | Fail
            | Undefined

type outcome = { status:  status;
                 expect:  expectation;
                 details: string
               }

module type DOMAIN =
  sig
    type t

    val arbitrary : int -> t option
    val describe : t -> Oqc_descr.t
    val description : Oqc_descr.t
  end

module type PROPERTY =
  sig
    module D : DOMAIN
    val predicate : D.t -> outcome
    val description : Oqc_descr.t -> Oqc_descr.t
  end

module type ATOMIC_PROPERTY = PROPERTY with type D.t = unit

module type OPERATION =
  sig
    type src
    type dst

    val f : src -> dst
    val description : Oqc_descr.t -> Oqc_descr.t
  end

module EmptyDomain =
  struct
    [@@@ocaml.warning "-37"]
    type t = NonExistent

    let arbitrary _ = None
    let describe _ = assert false
    let description = Oqc_descr.atom "∅"
  end

module UnitDomain =
  struct
    type t = unit

    let arbitrary _ = Some ()
    let describe () = Oqc_descr.atom "()"
    let description = Oqc_descr.atom "Unit"
  end

module AlwaysPass =
  struct
    module D = UnitDomain

    let predicate () = { status = Pass; expect = Expected; details = "ok" }

    let description _ = Oqc_descr.atom "TRUE"
  end

module AlwaysFail =
  struct
    module D = UnitDomain

    let predicate () = { status = Fail; expect = Unexpected; details = "fail" }

    let description _ = Oqc_descr.atom "FALSE"
  end

module AlwaysUndefined =
  struct
    module D = UnitDomain

    let predicate () = { status = Undefined; expect = Unexpected;
                         details =  "undefined" }

    let description _ = Oqc_descr.atom "UNDEF"
  end

module ExpectFailure (P : PROPERTY) =
  struct
    module D = P.D

    let predicate v =
      let r = P.predicate v in
      match r.status with
      | Fail -> { r with expect = Expected }
      | Pass | Undefined -> { r with expect = Unexpected }

    let description arg = P.description arg
  end

module ExpectUndefined (P : PROPERTY) =
  struct
    module D = P.D

    let predicate v =
      let r = P.predicate v in
      match r.status with
      | Undefined -> { r with expect = Expected }
      | Fail | Pass -> { r with expect = Unexpected }

    let description arg = P.description arg
  end

module Required (P : PROPERTY) =
  struct
    module D = P.D

    let predicate v =
      let r = P.predicate v in
      match r.status with
      | Undefined | Fail -> { r with expect = Unexpected; status = Fail }
      | Pass -> r

    let description arg = P.description arg
  end

module ConstPred (D : DOMAIN) (P : ATOMIC_PROPERTY) =
  struct
    module D = D

    let predicate _ = P.predicate ()

    let description _ = P.description (Oqc_descr.atom "")
  end

module Restriction (D : DOMAIN) (P : PROPERTY with type D.t = D.t) =
  struct
    module D = D

    let predicate x = P.predicate x

    let description x = P.description x
  end

module Identity (D1 : DOMAIN) (D2 : DOMAIN with type t = D1.t) =
  struct
    type src = D1.t
    type dst = D2.t

    let f x = x

    let description x = Oqc_descr.wrap "("  x ")"
  end

let ensure message b =
  if b then
    { status = Pass; expect = Expected; details = "success" }
  else
    { status = Fail; expect = Unexpected; details = message }

let all_properties : (module ATOMIC_PROPERTY) Queue.t = Queue.create () ;;

let register (m : (module ATOMIC_PROPERTY)) =
  Queue.add m all_properties ;;

let do_log msg = Oqc_tap.comment msg

let fatal_failures = ref false

let fail_unexpected = ref false

let allow_expected_failures = ref true

let catch_exceptions = ref true

let run () =
  Oqc_tap.plan (Queue.length all_properties);
  let normalize r =
    match r.status, r.expect with
    | Fail, Expected when not !allow_expected_failures ->
       { r with expect = Unexpected }
    | (Pass | Undefined), Unexpected when !fail_unexpected ->
       { r with status = Fail }
    | (Fail | Pass | Undefined), (Expected | Unexpected) -> r
  in
  try
    for i = 1 to Queue.length all_properties do
      let m = Queue.take all_properties in
      let module Prop = (val m : ATOMIC_PROPERTY) in
      let descr = Oqc_descr.to_string (Prop.description (Oqc_descr.atom "")) in
      match normalize (Prop.predicate ()) with
      | { status = Pass; expect = Expected; _ } ->
         Oqc_tap.test ~ord: i ~description: descr (Ok ())
      | { status = Pass; expect = Unexpected;
          details = msg }
        | { status = Fail; expect = Expected;
            details = msg } -> Oqc_tap.test ~ord: i
                                 ~description: descr
                                 ~directive: (Oqc_tap.Todo msg)
                                 (Error ())
      | { status = Fail; expect = Unexpected;
          details = msg } when !fatal_failures -> raise (Failure msg)
      | { status = Fail; expect = Unexpected;
          details = msg } -> Oqc_tap.test ~ord: i
                               ~description: (descr ^ ": " ^ msg)
                               (Error ())
      | { status = Undefined; details = msg; _ } ->
         Oqc_tap.test ~ord: i
           ~description: descr
           ~directive: (Oqc_tap.Skip msg)
           (Ok ())

    done
  with
    ex when !catch_exceptions -> Oqc_tap.bailout (Printexc.to_string ex)

let min_iterations = ref 100

let max_attempts = ref 100

let sample_size = ref 10

let verbose = ref false

let do_verbose_log msg = if !verbose then do_log msg else ()
