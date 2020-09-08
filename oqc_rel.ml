open Oqc
open Oqc_iterate
open Oqc_domains

module Equality (D : DOMAIN) =
  struct
    module D = DomainPair (D) (D)

    let predicate (x, y) = ensure "not equal" (x = y)

    let description d = Oqc_descr.infix (Oqc_descr.prefix (-1, "π₁") d)
                      (false, 50, " = ") (Oqc_descr.prefix (-1, "π₂") d)
  end

module DiagonalDomain (D : DOMAIN) =
  struct
    type t = D.t * D.t

    let arbitrary size =
      match D.arbitrary size with
      | None -> None
      | Some x -> Some (x, x)

    module DP = DomainPair (D) (D)
    let describe x = DP.describe x

    let description = Oqc_descr.prefix (-1, "diag") D.description
  end

module IsReflexive (D : DOMAIN) (R : PROPERTY with type D.t = D.t * D.t) =
  Forall (Restriction (DiagonalDomain (D)) (R))
