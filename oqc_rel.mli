open Oqc
open Oqc_domains

module Equality (D : DOMAIN) : PROPERTY with module D = DomainPair (D) (D)

module DiagonalDomain (D : DOMAIN) : DOMAIN with type t = D.t * D.t

module IsReflexive (D : DOMAIN) (R : PROPERTY with type D.t = D.t * D.t) : ATOMIC_PROPERTY
