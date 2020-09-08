open Oqc
open Oqc_logic
open Oqc_domains
open Oqc_iterate
open Oqc_rel ;;

List.iter register [(module AlwaysPass : ATOMIC_PROPERTY);
                    (module ExpectFailure (AlwaysFail) : ATOMIC_PROPERTY);
                    (module ExpectUndefined (AlwaysUndefined) : ATOMIC_PROPERTY);
                    (module ExpectFailure (ExpectFailure (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (ExpectFailure (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (ExpectUndefined (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (ExpectUndefined (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectUndefined (ExpectUndefined (AlwaysUndefined)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (ExpectUndefined (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectUndefined (ExpectFailure (AlwaysUndefined)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (Required (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (Required (AlwaysUndefined)) : ATOMIC_PROPERTY);
                    (module Required (NotProp (NotProp (AlwaysPass))) : ATOMIC_PROPERTY);
                    (module ExpectFailure (NotProp (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectUndefined (NotProp (AlwaysUndefined)) : ATOMIC_PROPERTY);
                    (module Required (AndProp (AlwaysPass) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (AndProp (AlwaysFail) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (AndProp (AlwaysPass) (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectUndefined (AndProp (AlwaysUndefined) (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectUndefined (AndProp (AlwaysPass) (AlwaysUndefined)) : ATOMIC_PROPERTY);
                    (module Required (OrProp (AlwaysPass) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module Required (OrProp (AlwaysFail) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module Required (OrProp (AlwaysPass) (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module Required (CondProp (AlwaysPass) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (CondProp (AlwaysPass) (AlwaysFail)) : ATOMIC_PROPERTY);
                    (module ExpectUndefined (CondProp (AlwaysFail) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectUndefined (CondProp (AlwaysUndefined) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module Required (CondProp (AlwaysPass) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module Required (EquivProp (AlwaysPass) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module Required (EquivProp (AlwaysFail) (NotProp (AlwaysPass))) : ATOMIC_PROPERTY);
                    (module ExpectFailure (EquivProp (AlwaysUndefined) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (EquivProp (AlwaysFail) (AlwaysUndefined)) : ATOMIC_PROPERTY);
                    (module ExpectFailure (EquivProp (NotProp (AlwaysPass)) (AlwaysPass)) : ATOMIC_PROPERTY);
                    (module ExpectUndefined (EquivProp (NotProp (AlwaysUndefined)) (AlwaysUndefined)) : ATOMIC_PROPERTY) ] ;;


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

List.iter (fun dom ->
    let module M = (val dom : DOMAIN) in
    register (module Required (Forall
                                 (Restriction
                                    (DiagonalDomain (M))
                                    (Equality (M))))))
  [(module UnitDomain : DOMAIN);
   (module IntegerDom : DOMAIN); (module NaturalDom : DOMAIN);
   (module CharDom : DOMAIN); (module StringDom : DOMAIN);
   (module BooleanDom : DOMAIN); (module FloatDom : DOMAIN);
   (module DomainPair (IntegerDom) (IntegerDom) : DOMAIN);
   (module DomainTriplet (IntegerDom) (IntegerDom) (IntegerDom) : DOMAIN);
   (module DomainList (IntegerDom) : DOMAIN);
   (module DomainArray (IntegerDom) : DOMAIN);
   (module DomainDisjointUnion (IntegerDom) (StringDom) : DOMAIN)] ;;

register (module Required (Forall (Equality (UnitDomain)))) ;;

List.iter (fun dom ->
    let module M = (val dom : DOMAIN) in
    register (module Required (Exists
                                 (NotProp (Equality (M))))))
  [(module IntegerDom : DOMAIN); (module NaturalDom : DOMAIN);
   (module CharDom : DOMAIN); (module StringDom : DOMAIN);
   (module BooleanDom : DOMAIN); (module FloatDom : DOMAIN);
   (module DomainPair (IntegerDom) (IntegerDom) : DOMAIN);
   (module DomainTriplet (IntegerDom) (IntegerDom) (IntegerDom) : DOMAIN);
   (module DomainList (IntegerDom) : DOMAIN);
   (module DomainArray (IntegerDom) : DOMAIN);
   (module DomainDisjointUnion (IntegerDom) (StringDom) : DOMAIN)] ;;


register (module (ExpectUndefined (Forall (ConstPred (EmptyDomain) (AlwaysPass))))) ;;
register (module (ExpectUndefined (Forall (ConstPred (EmptyDomain) (AlwaysFail))))) ;;

register (module (ExpectFailure (Forall (ConstPred (IntegerDom) (AlwaysFail))))) ;;
register (module (ExpectFailure (Exists (ConstPred (IntegerDom) (AlwaysFail))))) ;;
register (module (ExpectFailure (ExistsUnique (ConstPred (IntegerDom) (AlwaysFail))))) ;;

register (module (ExpectUndefined (Forall (ConstPred (IntegerDom) (AlwaysUndefined))))) ;;
register (module (ExpectUndefined (Exists (ConstPred (IntegerDom) (AlwaysUndefined))))) ;;
register (module (ExpectUndefined (ExistsUnique (ConstPred (IntegerDom) (AlwaysUndefined))))) ;;

register (module (Required (ExistDistinct (ConstPred (BooleanDom) (AlwaysPass))))) ;;
register (module (Required (ExistDistinct (ConstPred (DomainOption (UnitDomain)) (AlwaysPass))))) ;;
register (module (ExpectUndefined (ExistDistinct (ConstPred (UnitDomain) (AlwaysPass))))) ;;

module EvenIntPredicate =
  struct
    module D = IntegerDom
    let predicate v = ensure "odd value" (v mod 2 = 0)

    let description arg = Oqc_descr.prefix (-1, "Is-even ") arg
  end ;;

register (module Required (Exists (EvenIntPredicate))) ;;
register (module ExpectFailure (Forall (EvenIntPredicate))) ;;
register (module ExpectFailure (ExistsUnique (EvenIntPredicate))) ;;

register (module Required (ExistsUnique (AlwaysPass))) ;;

register (module ExpectUndefined (Forall (ConstPred (EmptyDomain) (AlwaysPass)))) ;;
register (module ExpectFailure (Exists (ConstPred (EmptyDomain) (AlwaysPass)))) ;;

register (module ExpectUndefined (Forall (ConstPred (DomainPair (EmptyDomain) (IntegerDom)) (AlwaysPass)))) ;;
register (module ExpectUndefined (Forall (ConstPred (DomainTriplet (BooleanDom) (EmptyDomain) (IntegerDom)) (AlwaysPass)))) ;;

register (module Required (ExistsUnique (ConstPred (DomainList (EmptyDomain)) (AlwaysPass)))) ;;
register (module Required (ExistsUnique (ConstPred (DomainArray (EmptyDomain)) (AlwaysPass)))) ;;
register (module Required (Exists (ConstPred (DomainDisjointUnion (EmptyDomain) (UnitDomain)) (AlwaysPass)))) ;;
register (module Required (Exists (ConstPred (DomainDisjointUnion (UnitDomain) (EmptyDomain)) (AlwaysPass)))) ;;
register (module ExpectUndefined (Exists (ConstPred (DomainDisjointUnion (EmptyDomain) (EmptyDomain)) (AlwaysPass)))) ;;
register (module Required (ExistDistinct (ConstPred (DomainDisjointUnion (UnitDomain) (UnitDomain)) (AlwaysPass)))) ;;

register (module Required (ExistsUnique (ConstPred (DomainUnion (UnitDomain) (UnitDomain)) (AlwaysPass)))) ;;
register (module Required (ExistsUnique (ConstPred (DomainUnion (SubDomain (UnitDomain) (AlwaysFail)) (UnitDomain)) (AlwaysPass)))) ;;
register (module Required (ExistsUnique (ConstPred (DomainUnion (UnitDomain) (SubDomain (UnitDomain) (AlwaysFail))) (AlwaysPass)))) ;;
register (module ExpectUndefined (Exists (ConstPred (DomainUnion (EmptyDomain) (EmptyDomain)) (AlwaysPass)))) ;;

register (module Required (Exists (ConstPred (SubDomain (IntegerDom) (EvenIntPredicate)) (AlwaysPass)))) ;;
register (module Required (Forall (Restriction (SubDomain (IntegerDom) (EvenIntPredicate)) (EvenIntPredicate)))) ;;

register (module ExpectUndefined (Forall (ConstPred (SubDomain (EmptyDomain) (ConstPred (EmptyDomain) (AlwaysPass))) (AlwaysPass)))) ;;

register (module Required (ExistsUnique (ConstPred (DomainImage (UnitDomain) (UnitDomain) (Identity (UnitDomain) (UnitDomain))) (AlwaysPass)))) ;;
register (module ExpectUndefined (Exists (ConstPred (DomainImage (EmptyDomain) (EmptyDomain) (Identity (EmptyDomain) (EmptyDomain))) (AlwaysPass)))) ;;

register (module Required (IsReflexive (IntegerDom) (Equality (IntegerDom)))) ;;

Arg.parse ["-min-iterations", Arg.Set_int min_iterations,
           "Mininum required number of iterations";
           "-max-attempts", Arg.Set_int max_attempts,
           "Maximum number of attempts to produce a value";
           "-sample-size", Arg.Set_int sample_size,
           "Size factor for generated data structures";
           "-verbose", Arg.Set verbose,
           "Show verbose messages";
           "-fail-unexpected",
           Arg.Set fail_unexpected,
           "Report all unexpected results as failures";
           "-no-expected-failures",
           Arg.Clear allow_expected_failures,
           "Treat expected failures as unexpected";
           "-fatal-failures",
           Arg.Set fatal_failures,
           "Make all unexpected failures fatal";
           "-dont-catch-exceptions",
           Arg.Clear catch_exceptions,
           "Do not catch exceptions at the top level"]
  (fun _ -> raise (Arg.Bad "Invalid argument"))
  "Self-test for OQC" ;;

Random.self_init () ;;
run ()
