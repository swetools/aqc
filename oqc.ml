type outcome = Pass
             | Fail of string
             | Skip of string
             | XFail of string

module type DOMAIN =
  sig
    type t

    val arbitrary : int -> t option
    val to_descr : t -> Oqc_descr.t
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
    let to_descr _ = assert false
    let description = Oqc_descr.atom "∅"
  end

module UnitDomain =
  struct
    type t = unit

    let arbitrary _ = Some ()
    let to_descr () = Oqc_descr.atom "()"
    let description = Oqc_descr.atom "Unit"
  end

module AlwaysPass =
  struct
    module D = UnitDomain

    let predicate () = Pass

    let description _ = Oqc_descr.atom "TRUE"
  end

module AlwaysFail =
  struct
    module D = UnitDomain

    let predicate () = Fail "fail"

    let description _ = Oqc_descr.atom "FALSE"
  end

module AlwaysSkip =
  struct
    module D = UnitDomain

    let predicate () = Skip "skip"

    let description _ = Oqc_descr.atom "SKIP"
  end

let expect_failures = ref true

module ExpectFailure (P : PROPERTY) =
  struct
    module D = P.D

    let predicate v =
      match P.predicate v with
      | Pass -> Fail "unexpected success"
      | Skip _ as r -> r
      | Fail msg | XFail msg when !expect_failures -> XFail msg
      | Fail msg | XFail msg -> Fail msg

    let description arg = P.description arg
  end

module ExpectSkipped (P : PROPERTY) =
  struct
    module D = P.D

    let predicate v =
      match P.predicate v with
      | Skip _ as r -> r
      | Fail msg | XFail msg -> Fail msg
      | Pass -> Fail "should be skipped"

    let description arg = P.description arg
  end

module Required (P : PROPERTY) =
  struct
    module D = P.D

    let predicate v =
      match P.predicate v with
      | Skip msg | XFail msg | Fail msg -> Fail msg
      | Pass -> Pass

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
  if b then Pass
  else
    Fail message

let all_properties : (module ATOMIC_PROPERTY) Queue.t = Queue.create () ;;

let register (m : (module ATOMIC_PROPERTY)) =
  Queue.add m all_properties ;;

let do_log msg = Oqc_tap.comment msg

let fatal_failures = ref false

let run () =
  Oqc_tap.plan (Queue.length all_properties);
  try
    for i = 1 to Queue.length all_properties do
      let m = Queue.take all_properties in
      let module Prop = (val m : ATOMIC_PROPERTY) in
      let descr = Oqc_descr.to_string (Prop.description (Oqc_descr.atom "")) in
      match Prop.predicate () with
      | Pass -> Oqc_tap.test ~ord: i
                  ~description: descr
                  (Ok ())
      | Fail msg when !fatal_failures -> raise (Failure msg)
      | Fail msg -> Oqc_tap.test ~ord: i
                      ~description: (descr ^ ": " ^ msg)
                      (Error ())
      | Skip msg -> Oqc_tap.test ~ord: i
                      ~description: descr
                      ~directive: (Oqc_tap.Skip msg)
                      (Ok ())
      | XFail msg -> Oqc_tap.test ~ord: i
                       ~description: descr
                       ~directive: (Oqc_tap.Todo msg)
                       (Error ())
    done
  with
    ex -> Oqc_tap.bailout (Printexc.to_string ex)

let min_iterations = ref 100

let max_attempts = ref 100

let sample_size = ref 10

let verbose = ref false

let do_verbose_log msg = if !verbose then do_log msg else ()
