open Oqc
open Oqc_domains

module Equality (D : DOMAIN) : PROPERTY with D = DomainPair (D) (D)
