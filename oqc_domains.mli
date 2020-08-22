open Oqc

module Integer : DOMAIN
module Natural : DOMAIN
module Char : DOMAIN
module Boolean : DOMAIN
module String : DOMAIN
module Float : DOMAIN

module Pair (D1 : DOMAIN) (D2 : DOMAIN) : DOMAIN
module Triplet (D1 : DOMAIN) (D2 : DOMAIN) (D3 : DOMAIN) : DOMAIN

module List (D : DOMAIN) : DOMAIN
module Array (D : DOMAIN) : DOMAIN

module DisjointUnion (D1 : DOMAIN) (D2 : DOMAIN) : DOMAIN
module Union (D1 : DOMAIN) (D2 : DOMAIN with type t = D1.t) : DOMAIN

module SubDomain (D : DOMAIN) (P : PREDICATE with type D.t = D.t) : DOMAIN
