open Oqc
open Oqc_logic
open Oqc_domains
open Oqc_iterate

module TrivialTest =
  struct
    let check () = Pass

    let description = "Trivial test"
  end ;;

register (module TrivialTest) ;;

register (module NotProp (NotProp (TrivialTest))) ;;

register (module ExpectFailure (NotProp (TrivialTest))) ;;

register (module AndProp (TrivialTest) (TrivialTest)) ;;

register (module ExpectFailure (AndProp (NotProp (TrivialTest))
                                  (TrivialTest))) ;;

register (module OrProp (TrivialTest) (TrivialTest)) ;;

register (module OrProp (NotProp (TrivialTest)) (TrivialTest)) ;;

register (module CondProp (TrivialTest) (TrivialTest)) ;;

register (module Required (CondProp (TrivialTest) (TrivialTest))) ;;

register (module ExpectSkipped (CondProp (NotProp (TrivialTest))
                                  (TrivialTest))) ;;

register (module EquivProp (TrivialTest) (TrivialTest)) ;;

register (module EquivProp (NotProp (TrivialTest)) (NotProp (TrivialTest))) ;;

register (module ExpectFailure (EquivProp (NotProp (TrivialTest))
                                  (TrivialTest))) ;;

module TrivialPredicate =
  struct
    module D = BooleanDom
    let predicate _ = Pass

    let description arg = "Trivial predicate (" ^ arg ^ ")"
  end ;;

register (module Forall (TrivialPredicate)) ;;

module TrivialNatPredicate =
  struct
    module D = NaturalDom
    let predicate v = ensure ~message: "negative value" (v >= 0)

    let description arg = "Trivial natural predicate (" ^ arg ^ ")"
  end ;;

register (module Forall (TrivialNatPredicate)) ;;

run ();
