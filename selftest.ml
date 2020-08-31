open Oqc
open Oqc_logic
open Oqc_domains
open Oqc_iterate ;;

List.iter register [(module AlwaysPass : ATOMIC_PROPERTY);
                    (module ExpectFailure (AlwaysFail) : ATOMIC_PROPERTY);
                    (module ExpectSkipped (AlwaysSkip) : ATOMIC_PROPERTY);
                    (module ExpectFailure (ExpectFailure (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (ExpectFailure (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (ExpectSkipped (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (ExpectSkipped (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectSkipped (ExpectSkipped (AlwaysSkip)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (ExpectSkipped (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectSkipped (ExpectFailure (AlwaysSkip)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (Required (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (Required (AlwaysSkip)) : ATOMIC_PROPERTY);
                    (module Required (NotProp (NotProp (AlwaysPass))) : ATOMIC_PROPERTY);
                    (module ExpectFailure (NotProp (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectSkipped (NotProp (AlwaysSkip)) : ATOMIC_PROPERTY);
                    (module Required (AndProp (AlwaysPass) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (AndProp (AlwaysFail) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (AndProp (AlwaysPass) (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectSkipped (AndProp (AlwaysSkip) (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectSkipped (AndProp (AlwaysPass) (AlwaysSkip)) : ATOMIC_PROPERTY);
                    (module Required (OrProp (AlwaysPass) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module Required (OrProp (AlwaysFail) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module Required (OrProp (AlwaysPass) (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module Required (CondProp (AlwaysPass) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (CondProp (AlwaysPass) (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectSkipped (CondProp (AlwaysFail) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectSkipped (CondProp (AlwaysSkip) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module Required (CondProp (AlwaysPass) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module Required (EquivProp (AlwaysPass) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module Required (EquivProp (AlwaysFail) (NotProp (AlwaysPass))) : ATOMIC_PROPERTY);
                    (module ExpectSkipped (EquivProp (AlwaysSkip) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectSkipped (EquivProp (AlwaysFail) (AlwaysSkip)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (EquivProp (NotProp (AlwaysPass)) (AlwaysPass)) : ATOMIC_PROPERTY)] ;;


List.iter (fun dom ->
    register (module Required (Forall (ConstPred ((val dom : DOMAIN)) (AlwaysPass)))))
  [(module UnitDomain : DOMAIN);
   (module IntegerDom : DOMAIN); (module NaturalDom : DOMAIN);
   (module CharDom : DOMAIN); (module StringDom : DOMAIN);
   (module BooleanDom : DOMAIN); (module FloatDom : DOMAIN);
   (module DomainPair (IntegerDom) (IntegerDom) : DOMAIN);
   (module DomainTriplet (IntegerDom) (IntegerDom) (IntegerDom) : DOMAIN);
   (module DomainList (IntegerDom) : DOMAIN);
   (module DomainArray (IntegerDom) : DOMAIN);
   (module DomainDisjointUnion (IntegerDom) (StringDom) : DOMAIN)] ;;

register (module (ExpectSkipped (Forall (ConstPred (EmptyDomain) (AlwaysPass))))) ;;
register (module (ExpectSkipped (Forall (ConstPred (EmptyDomain) (AlwaysFail))))) ;;

register (module (ExpectFailure (Forall (ConstPred (IntegerDom) (AlwaysFail))))) ;;
register (module (ExpectFailure (Exists (ConstPred (IntegerDom) (AlwaysFail))))) ;;
register (module (ExpectFailure (ExistsUnique (ConstPred (IntegerDom) (AlwaysFail))))) ;;

register (module (ExpectSkipped (Forall (ConstPred (IntegerDom) (AlwaysSkip))))) ;;
register (module (ExpectSkipped (Exists (ConstPred (IntegerDom) (AlwaysSkip))))) ;;
register (module (ExpectSkipped (ExistsUnique (ConstPred (IntegerDom) (AlwaysSkip))))) ;;

register (module (Required (ExistDistinct (ConstPred (BooleanDom) (AlwaysPass))))) ;;
register (module (Required (ExistDistinct (ConstPred (DomainOption (UnitDomain)) (AlwaysPass))))) ;;
register (module (ExpectSkipped (ExistDistinct (ConstPred (UnitDomain) (AlwaysPass))))) ;;

module EvenIntPredicate =
  struct
    module D = IntegerDom
    let predicate v = ensure "odd value" (v mod 2 = 0)

    let description arg = Oqc_descr.prefix (1, "Is-even") arg
  end ;;

register (module Required (Exists (EvenIntPredicate))) ;;
register (module ExpectFailure (Forall (EvenIntPredicate))) ;;
register (module ExpectFailure (ExistsUnique (EvenIntPredicate))) ;;

register (module Required (ExistsUnique (AlwaysPass))) ;;

register (module ExpectSkipped (Forall (ConstPred (EmptyDomain) (AlwaysPass)))) ;;
register (module ExpectFailure (Exists (ConstPred (EmptyDomain) (AlwaysPass)))) ;;

register (module ExpectSkipped (Forall (ConstPred (DomainPair (EmptyDomain) (IntegerDom)) (AlwaysPass)))) ;;
register (module ExpectSkipped (Forall (ConstPred (DomainTriplet (BooleanDom) (EmptyDomain) (IntegerDom)) (AlwaysPass)))) ;;

register (module Required (ExistsUnique (ConstPred (DomainList (EmptyDomain)) (AlwaysPass)))) ;;
register (module Required (ExistsUnique (ConstPred (DomainArray (EmptyDomain)) (AlwaysPass)))) ;;
register (module Required (Exists (ConstPred (DomainDisjointUnion (EmptyDomain) (UnitDomain)) (AlwaysPass)))) ;;
register (module Required (Exists (ConstPred (DomainDisjointUnion (UnitDomain) (EmptyDomain)) (AlwaysPass)))) ;;
register (module ExpectSkipped (Exists (ConstPred (DomainDisjointUnion (EmptyDomain) (EmptyDomain)) (AlwaysPass)))) ;;
register (module Required (ExistDistinct (ConstPred (DomainDisjointUnion (UnitDomain) (UnitDomain)) (AlwaysPass)))) ;;

register (module Required (ExistsUnique (ConstPred (DomainUnion (UnitDomain) (UnitDomain)) (AlwaysPass)))) ;;
register (module Required (ExistsUnique (ConstPred (DomainUnion (SubDomain (UnitDomain) (AlwaysFail)) (UnitDomain)) (AlwaysPass)))) ;;
register (module Required (ExistsUnique (ConstPred (DomainUnion (UnitDomain) (SubDomain (UnitDomain) (AlwaysFail))) (AlwaysPass)))) ;;
register (module ExpectSkipped (Exists (ConstPred (DomainUnion (EmptyDomain) (EmptyDomain)) (AlwaysPass)))) ;;

register (module Required (Exists (ConstPred (SubDomain (IntegerDom) (EvenIntPredicate)) (AlwaysPass)))) ;;
register (module Required (Forall (Restriction (SubDomain (IntegerDom) (EvenIntPredicate)) (EvenIntPredicate)))) ;;

register (module ExpectSkipped (Forall (ConstPred (SubDomain (EmptyDomain) (ConstPred (EmptyDomain) (AlwaysPass))) (AlwaysPass)))) ;;

register (module Required (ExistsUnique (ConstPred (MapDomain (UnitDomain) (UnitDomain) (Identity (UnitDomain) (UnitDomain))) (AlwaysPass)))) ;;
register (module ExpectSkipped (Exists (ConstPred (MapDomain (EmptyDomain) (EmptyDomain) (Identity (EmptyDomain) (EmptyDomain))) (AlwaysPass)))) ;;

Arg.parse ["-min-iterations", Arg.Set_int min_iterations,
           "Mininum required number of iterations";
           "-max-attempts", Arg.Set_int max_attempts,
           "Maximum number of attempts to produce a value";
           "-sample-size", Arg.Set_int sample_size,
           "Size factor for generated data structures";
           "-verbose", Arg.Set verbose,
           "Show verbose messages";
           "-dont-expect-failures",
           Arg.Clear expect_failures,
           "Report all failures as unexpected";
           "-fatal-failures",
           Arg.Set fatal_failures,
           "Make all failures fatal"] (fun _ -> raise (Arg.Bad "Invalid argument"))
  "Self-test for OQC" ;;

Random.self_init () ;;
run ()
