open Oqc

module IntegerDom : DOMAIN with type t = int
module NaturalDom : DOMAIN with type t = int
module CharDom : DOMAIN  with type t = char
module BooleanDom : DOMAIN with type t = bool
module StringDom : DOMAIN with type t = string
module FloatDom : DOMAIN with type t = float

module DomainPair (D1 : DOMAIN) (D2 : DOMAIN) : DOMAIN with type t = D1.t * D2.t

module DomainTriplet (D1 : DOMAIN) (D2 : DOMAIN) (D3 : DOMAIN) : DOMAIN
       with type t = D1.t * D2.t * D3.t

module DomainOption (D : DOMAIN) : DOMAIN with type t = D.t option

module DomainList (D : DOMAIN) : DOMAIN with type t = D.t list
module DomainArray (D : DOMAIN) : DOMAIN with type t = D.t array

type ('a, 'b) disjoint_union = Left of 'a
                             | Right of 'b

module DomainDisjointUnion (D1 : DOMAIN) (D2 : DOMAIN) : DOMAIN
       with type t = (D1.t, D2.t) disjoint_union
module DomainUnion (D1 : DOMAIN) (D2 : DOMAIN with type t = D1.t) : DOMAIN
       with type t = D1.t

module SubDomain (D : DOMAIN) (P : PROPERTY with type D.t = D.t) : DOMAIN
       with type t = D.t

module MapDomain (D1 : DOMAIN) (D2 : DOMAIN)
         (Op : OPERATION with type src = D1.t and type dst = D2.t) : DOMAIN
       with type t = D2.t
