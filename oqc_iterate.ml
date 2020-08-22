open Oqc

let exhausted n = Skip ("Arguments exhausted after " ^ string_of_int n ^ " attempts")

let for_all ?sample_size: (size = !sample_size) gen check =
  let rec iterate n m =
    if n >= !min_iterations then Pass
    else if m >= !max_attempts then
      exhausted m
    else match check (gen size) with
         | Pass | XFail _ -> iterate (succ n) 0
         | Skip _ -> iterate n (succ m)
         | Fail msg -> Fail msg
  in
  try
    iterate 0 0
  with
  | Exhausted (n, _) -> exhausted n

let for_some ?sample_size: (size = !sample_size)
             ?to_string: (to_string = fun _ -> "...")
             gen check =
  let rec iterate n m =
    if n >= !min_iterations then
      Fail ("No solutions after " ^ string_of_int n ^ " guesses")
    else if m >= !max_attempts then
      Skip ("Arguments exhausted after " ^ string_of_int m ^ " attempts")
    else let v = gen size in
         match check v with
         | Pass | XFail _ ->
                   do_log ("found solution: " ^ to_string v);
                   Pass
         | Skip _ -> iterate n (succ m)
         | Fail _ ->iterate (succ n) 0
  in
  try
    iterate 0 0
  with
  | Exhausted (n, _) -> exhausted n

let for_one ?sample_size: (size = !sample_size)
            ?to_string: (to_string = fun _ -> "...")
            gen check =
  let rec iterate found n m =
    if n >= !min_iterations then
      match found with
      | None -> Fail ("No solutions after " ^ string_of_int n ^ " guesses")
      | Some v ->
         do_log ("found solution: " ^ to_string v);
         Pass
    else if m >= !max_attempts then
      Skip ("Arguments exhausted after " ^ string_of_int m ^ " attempts")
    else let v = gen size in
         match check v with
         | Pass | XFail _ ->
                   (match found with
                    | Some v0 when v0 <> v ->
                       Fail ("duplicate solutions: " ^
                               to_string v ^ " and " ^
                                 to_string v0)
                    | None | Some _ ->
                              iterate (Some v) (succ n) 0)
         | Skip _ -> iterate found n (succ m)
         | Fail _ ->iterate found (succ n) 0
  in
  try
    iterate None 0 0
  with
  | Exhausted (n, _) -> exhausted n

module Forall (P : PREDICATE) =
  struct
    let check () = for_all P.D.arbitrary P.predicate

    let description = "∀ x : " ^ P.D.description ^ ": " ^ P.description "x"
  end

module Exists (P : PREDICATE) =
  struct
    let check () = for_some ~to_string: P.D.to_string P.D.arbitrary P.predicate

    let description = "∃ x :  " ^ P.D.description ^ ": " ^ P.description "x"
  end

module ExistsUnique (P : PREDICATE)  =
  struct
    let check () = for_one ~to_string: P.D.to_string P.D.arbitrary P.predicate

    let description = "∃! x :  " ^ P.D.description ^ ": " ^ P.description "x"
  end

module Forall2 (R : RELATION) =
  struct
    module D = R.D1

    let predicate v = for_all R.D2.arbitrary (R.relation v)

    let description arg = "∀ y : " ^ R.D1.description ^ ": " ^ R.description arg "y"
  end

module Exists2 (R : RELATION) =
  struct
    module D = R.D1

    let predicate v = for_some ~to_string: R.D2.to_string R.D2.arbitrary (R.relation v)

    let description arg = "∃ y : " ^ R.D1.description ^ ": " ^ R.description arg "y"
  end

module ExistsUnique2 (R : RELATION) =
  struct
    module D = R.D1

    let predicate v = for_one ~to_string: R.D2.to_string R.D2.arbitrary (R.relation v)

    let description arg = "∃! y : " ^ R.D1.description ^ ": " ^ R.description arg "y"
  end
