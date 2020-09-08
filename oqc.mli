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

module EmptyDomain : DOMAIN

module UnitDomain : DOMAIN with type t = unit

module AlwaysPass : ATOMIC_PROPERTY

module AlwaysFail : ATOMIC_PROPERTY

module AlwaysUndefined : ATOMIC_PROPERTY

module ExpectFailure (P : PROPERTY) : PROPERTY with module D = P.D

module ExpectUndefined (P : PROPERTY) : PROPERTY  with module D = P.D

module Required (P : PROPERTY) : PROPERTY  with module D = P.D

module ConstPred (D : DOMAIN) (P : ATOMIC_PROPERTY) : PROPERTY
       with module D = D

module Restriction (D : DOMAIN) (P : PROPERTY with type D.t = D.t) : PROPERTY
       with module D = D

module Identity (D1 : DOMAIN) (D2 : DOMAIN with type t = D1.t) : OPERATION
       with type src = D1.t and type dst = D2.t

val ensure : string -> bool -> outcome

val register : (module ATOMIC_PROPERTY) -> unit

val do_log : string -> unit

val do_verbose_log : string -> unit

val run : unit -> unit

val min_iterations : int ref

val max_attempts : int ref

val sample_size : int ref

val verbose : bool ref

val fail_unexpected : bool ref

val allow_expected_failures : bool ref

val fatal_failures : bool ref

val catch_exceptions : bool ref
