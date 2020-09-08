open Oqc

module IntegerDom =
  struct
    type t = int

    let arbitrary _ = Some (Random.bits () - 0x10000000)

    let describe x = Oqc_descr.atom (string_of_int x)

    let description = Oqc_descr.atom "ℤ"
  end

module NaturalDom =
  struct
    type t = int

    let arbitrary _ = Some (Random.bits ())

    let describe x = Oqc_descr.atom (string_of_int x)

    let description = Oqc_descr.atom "ℕ"
  end

module CharDom =
  struct
    type t = char

    let arbitrary _ = Some (char_of_int (Random.int 256))

    let describe c = Oqc_descr.atom ("'" ^ Char.escaped c ^ "'")

    let description = Oqc_descr.atom "Char"
  end

module BooleanDom =
  struct
    type t = bool

    let arbitrary _ = Some (Random.bool ())

    let describe b = Oqc_descr.atom (string_of_bool b)

    let description = Oqc_descr.atom "Boolean"
  end

module StringDom =
  struct
    type t = string

    let arbitrary max_size =
      Some (String.init (Random.int max_size)
              (fun _ -> char_of_int (Random.int 256)))

    let describe s = Oqc_descr.atom ("\"" ^ String.escaped s ^ "\"")

    let description = Oqc_descr.atom "String"
  end

module FloatDom =
  struct
    type t = float

    let arbitrary _ =
      let v = Random.float max_float in
      Some (if Random.bool () then -. v else v)

    let describe v = Oqc_descr.atom (string_of_float v)

    let description = Oqc_descr.atom "ℝ"
  end

module DomainPair (D1 : DOMAIN) (D2 : DOMAIN) =
  struct
    type t = D1.t * D2.t

    let arbitrary size =
      match D1.arbitrary size, D2.arbitrary size with
      | Some a1, Some a2 -> Some (a1, a2)
      | None, _ | _, None -> None

    let describe (x, y) = Oqc_descr.wrap "("
                            (Oqc_descr.infix (D1.describe x)
                               (true, 40, ", ")
                               (D2.describe y)) ")"

    let description = Oqc_descr.infix D1.description (true, 10, " × ") D2.description
  end

module DomainTriplet (D1 : DOMAIN) (D2 : DOMAIN) (D3 : DOMAIN) =
  struct
    type t = D1.t * D2.t * D3.t

    let arbitrary size =
      match D1.arbitrary size, D2.arbitrary size, D3.arbitrary size with
      | Some a1, Some a2, Some a3 -> Some (a1, a2, a3)
      | None, _, _ | _, None, _ | _, _, None -> None

    let describe (x, y, z) = Oqc_descr.wrap "("
                               (Oqc_descr.infix (D1.describe x)
                                  (true, 40, ", ")
                                  (Oqc_descr.infix (D2.describe y)
                                     (true, 40, ", ")
                                     (D3.describe z))) ")"

    let description = Oqc_descr.infix D1.description (true, 10, " × ")
                        (Oqc_descr.infix D2.description (true, 10, " × ") D3.description)
  end

module DomainOption (D : DOMAIN) =
  struct
    type t = D.t option

    let arbitrary size =
      if Random.int (succ size) = 0 then Some None
      else Some (D.arbitrary size)

    let describe x =
      match x with
      | None -> Oqc_descr.atom "NULL"
      | Some v -> D.describe v

    let description = Oqc_descr.suffix D.description (5, "?")
  end

module DomainList (D : DOMAIN) =
  struct
    type t = D.t list

    let arbitrary size =
      let l = Random.int (succ size) in
      let rec generate n tail =
        if n = 0 then Some tail
        else
          match D.arbitrary (size / l) with
          | Some a -> generate (pred n) (a :: tail)
          | None -> Some []
      in
      generate l []

    let describe l =
      match l with
      | [] -> Oqc_descr.atom "[]"
      | hd :: tl -> Oqc_descr.wrap "["
                      (List.fold_left
                         (fun d x ->
                           Oqc_descr.infix d (true, 40, ", ")
                             (D.describe x))
                         (D.describe hd) tl) "]"
    let description = Oqc_descr.suffix D.description (5, "*")
  end

module DomainArray (D : DOMAIN) =
  struct
    type t = D.t array

    let arbitrary max_size =
      try
        let l = Random.int (succ max_size) in
        Some (Array.init l  (fun _ ->
                  match D.arbitrary (max_size / l) with
                  | Some a -> a
                  | None -> raise Exit))
      with
        Exit -> Some [||]

    let describe a =
      if Array.length a = 0 then
        Oqc_descr.atom "{}"
      else
        let rec fold n d =
          if n = Array.length a then d
          else
            let d0 = Oqc_descr.infix (Oqc_descr.atom (string_of_int n))
                       (false, 39, " ↦ ") (D.describe a.(n))
            in
            if n = 0 then
              fold 1 d0
            else
              fold (succ n) (Oqc_descr.infix d (true, 40, ", ") d0)
        in
        Oqc_descr.wrap "[" (fold 0 (Oqc_descr.atom "")) "]"

    let description = Oqc_descr.infix (Oqc_descr.atom "ℕ") (false, 400, " ↦ ") D.description
  end

type ('a, 'b) disjoint_union = Left of 'a
                             | Right of 'b

module DomainDisjointUnion (D1 : DOMAIN) (D2 : DOMAIN) =
  struct
    type t = (D1.t, D2.t) disjoint_union

    let arbitrary size =
      if Random.bool () then
        match D1.arbitrary size with
        | Some a -> Some (Left a)
        | None -> (match D2.arbitrary size with
                   | Some a -> Some (Right a)
                   | None -> None)
      else
        match D2.arbitrary size with
        | Some a -> Some (Right a)
        | None -> (match D1.arbitrary size with
                   | Some a -> Some (Left a)
                   | None -> None)

    let describe v =
      match v with
      | Left l -> Oqc_descr.infix (D1.describe l) (false, 50, " : ") D1.description
      | Right r -> Oqc_descr.infix (D2.describe r) (false, 50, " : ") D2.description

    let description = Oqc_descr.infix D1.description (true, 20, " ⊔ ") D2.description
  end

module DomainUnion (D1 : DOMAIN) (D2 : DOMAIN with type t = D1.t) =
  struct
    type t = D1.t

    let arbitrary size =
      if Random.bool () then D1.arbitrary size else D2.arbitrary size

    let describe = D1.describe

    let description = Oqc_descr.infix D1.description (true, 21, " ∪ ") D2.description
  end

module SubDomain (D : DOMAIN) (P : PROPERTY with type D.t = D.t) =
  struct
    type t = D.t

    let description =
      let v = Oqc_descr.freshvar (P.description (Oqc_descr.atom "")) in
      let mb = Oqc_descr.infix v (false, 50, " ∈ ") D.description in
      Oqc_descr.wrap "{" (Oqc_descr.infix mb (false, 1000, " | ")
                      (P.description v)) "}"

    let arbitrary size =
      let rec attempt n =
        if n >= !max_attempts then
          None
        else match D.arbitrary size with
             | Some v ->
                begin
                  match (P.predicate v).status with
                  | Pass -> Some v
                  | Fail | Undefined  ->
                     attempt (succ n)
                end
             | None -> None
      in
      attempt 0

    let describe = D.describe

  end

module DomainImage (D1 : DOMAIN) (D2 : DOMAIN)
         (Op : OPERATION with type src = D1.t and type dst = D2.t) =
  struct
    type t = D2.t

    let description = Op.description D1.description

    let arbitrary size =
      match D1.arbitrary size with
      | Some v -> Some (Op.f v)
      | None -> None

    let describe = D2.describe
  end
