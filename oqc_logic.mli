open Oqc

val (!!) : ('a -> outcome) -> ('a -> outcome)
val (&&&) : ('a -> outcome) -> ('a -> outcome) -> ('a -> outcome)
val (|||) : ('a -> outcome) -> ('a -> outcome) -> ('a -> outcome)
val (==>) : ('a -> outcome) -> ('a -> outcome) -> ('a -> outcome)
val (<==>) : ('a -> outcome) -> ('a -> outcome) -> ('a -> outcome)

module NotProp (P : PROPOSITION) : PROPOSITION
module AndProp (P1 : PROPOSITION) (P2 : PROPOSITION) : PROPOSITION
module OrProp (P1 : PROPOSITION) (P2 : PROPOSITION) : PROPOSITION
module CondProp (P1 : PROPOSITION) (P2 : PROPOSITION) : PROPOSITION
module EquivProp (P1 : PROPOSITION) (P2 : PROPOSITION) : PROPOSITION

module NotPred (P : PREDICATE) : PREDICATE
module AndPred (P1 : PREDICATE) (P2 : PREDICATE with type D.t = P1.D.t) : PREDICATE
module OrPred (P1 : PREDICATE) (P2 : PREDICATE with type D.t = P1.D.t) : PREDICATE
module CondPred (P1 : PREDICATE) (P2 : PREDICATE with type D.t = P1.D.t) : PREDICATE
module EquivPred (P1 : PREDICATE) (P2 : PREDICATE with type D.t = P1.D.t) : PREDICATE

module NotRel (R : RELATION) : RELATION
module AndRel (R1 : RELATION) (R2 : RELATION with type D1.t = R1.D1.t and type D2.t = R1.D2.t) : RELATION
module OrRel (R1 : RELATION) (R2 : RELATION with type D1.t = R1.D1.t and type D2.t = R1.D2.t) : RELATION
module CondRel (R1 : RELATION) (R2 : RELATION with type D1.t = R1.D1.t and type D2.t = R1.D2.t) : RELATION
module EquivRel (R1 : RELATION) (R2 : RELATION with type D1.t = R1.D1.t and type D2.t = R1.D2.t) : RELATION
