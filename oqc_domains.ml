open Oqc

module GString = String
       
module Integer =
  struct
    type t = int

    let arbitrary _ = Random.bits () - max_int / 2

    let to_string x = string_of_int x

    let description = "ℤ"
  end
    
module Natural =
  struct
    type t = int

    let arbitrary _ = Random.bits ()

    let to_string x = string_of_int x

    let description = "ℕ"
  end

module Char =
  struct
    type t = char

    let arbitrary _ = char_of_int (Random.int 256)

    let to_string c = Char.escaped c

    let description = "Char"
  end

module Boolean =
  struct
    type t = bool

    let arbitrary _ = Random.bool ()

    let to_string b = string_of_bool b

    let description = "Boolean"
  end
    
module String =
  struct
    type t = string

    let arbitrary max_size =
      String.init (Random.int max_size) (fun _ -> char_of_int (Random.int 256))

    let to_string s = String.escaped s
                  
    let description = "String"
  end
                  
module Float =
  struct
    type t = float

    let arbitrary _ =
      let v = Random.float max_float in
      if Random.bool () then -. v else v

    let to_string v = string_of_float v

    let description = "ℝ"
  end
              
module Pair (D1 : DOMAIN) (D2 : DOMAIN) =
  struct
    type t = D1.t * D2.t

    let arbitrary size = D1.arbitrary size, D2.arbitrary size

    let to_string (x, y) = "(" ^ D1.to_string x ^ "," ^ D2.to_string y ^ ")"

    let description = D1.description ^ " × " ^ D2.description
  end

module Triplet (D1 : DOMAIN) (D2 : DOMAIN) (D3 : DOMAIN) =
  struct
    type t = D1.t * D2.t * D3.t

    let arbitrary size = D1.arbitrary size, D2.arbitrary size, D3.arbitrary size

    let to_string (x, y, z) = "(" ^ D1.to_string x ^ "," ^ D2.to_string y ^ "," ^ D3.to_string z ^ ")"

    let description = D1.description ^ " × " ^ D2.description ^ " × " ^ D3.description
  end

  
module List (D : DOMAIN) =
  struct
    type t = D.t list

    let arbitrary size =
      let l = Random.int size in
      let rec generate n tail =
        if n = 0 then tail
        else generate (pred n) (D.arbitrary (size / l) :: tail)
      in
      generate l []

    let to_string l = "[" ^ GString.concat ", " (List.map D.to_string l) ^ "]"

    let description = "List of " ^ D.description
  end
                 
module Array (D : DOMAIN) =
  struct
    type t = D.t array

    let arbitrary max_size =
      let l = Random.int max_size in
      Array.init l  (fun _ -> D.arbitrary (max_size / l))

    let to_string a = "{" ^ GString.concat ", " (Array.to_list (Array.map D.to_string a)) ^ "}"
                  
    let description = "Array of " ^ D.description
  end
               
module DisjointUnion (D1 : DOMAIN) (D2 : DOMAIN) =
  struct
    type t = Left of D1.t
           | Right of D2.t

    let arbitrary size =
      if Random.bool () then Left (D1.arbitrary size) else Right (D2.arbitrary size)

    let to_string v =
      match v with
      | Left l -> D1.to_string l ^ " : " ^ D1.description
      | Right r -> D2.to_string r ^ " : " ^ D2.description

    let description = D1.description ^ " ⊔ " ^ D2.description
  end
                              
                        
module Union (D1 : DOMAIN) (D2 : DOMAIN with type t = D1.t) =
  struct
    type t = D1.t

    let arbitrary size =
      if Random.bool () then D1.arbitrary size else D2.arbitrary size

    let to_string = D1.to_string

    let description = D1.description ^ " ∪ " ^ D2.description
  end

module SubDomain (D : DOMAIN) (P : PREDICATE with type D.t = D.t) =
  struct
    type t = D.t

    let description = "{ x ∈ " ^ D.description ^ " | " ^ P.description "x" ^ "}"
               
    let arbitrary size =
      let rec attempt n =
        if n >= !max_attempts then
          raise (Exhausted (n, description))
        else let v = D.arbitrary size in
             match P.predicate v with
             | Pass | XFail _ -> v
             | Fail _ | Skip _ -> attempt (succ n)
      in
      attempt 0

    let to_string = D.to_string

  end

                                                                             
