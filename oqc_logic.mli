open Oqc

val (!!) : ('a -> outcome) -> ('a -> outcome)
val (&&&) : ('a -> outcome) -> ('a -> outcome) -> ('a -> outcome)
val (|||) : ('a -> outcome) -> ('a -> outcome) -> ('a -> outcome)
val (==>) : ('a -> outcome) -> ('a -> outcome) -> ('a -> outcome)
val (<==>) : ('a -> outcome) -> ('a -> outcome) -> ('a -> outcome)

module NotProp (P : PROPERTY) : PROPERTY with module D = P.D
module AndProp (P1 : PROPERTY) (P2 : PROPERTY with module D = P1.D) : PROPERTY with module D = P1.D
module OrProp (P1 : PROPERTY) (P2 : PROPERTY with module D = P1.D) : PROPERTY with module D = P1.D
module CondProp (P1 : PROPERTY) (P2 : PROPERTY with module D = P1.D) : PROPERTY with module D = P1.D
module EquivProp (P1 : PROPERTY) (P2 : PROPERTY with module D = P1.D) : PROPERTY with module D = P1.D
