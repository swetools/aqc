open Oqc

let search_solution ~to_string ~min_distinct ~max_distinct
      ~max_failures ~generator ~predicate =
  let rec iterate n m solutions failures =
    match () with
    | _ when List.length failures > max_failures ->
       { status = Fail; expect = Unexpected;
         details = String.concat ", " (List.map snd failures)
       }
    | _ when List.length solutions > max_distinct ->
       { status = Fail; expect = Unexpected;
         details = Printf.sprintf "%d solutions found, expected at most %d"
                     (List.length solutions) max_distinct
       }
    | _ when m >= !max_attempts ->
       { status = Undefined; expect = Unexpected;
         details = Printf.sprintf "Arguments exhausted after %d tries"
                     !max_attempts
       }
    | _ when n >= !min_iterations ->
       begin
         if List.length solutions < min_distinct then
           { status = Undefined; expect = Unexpected;
             details = Printf.sprintf "%d solutions found, expected at least %d"
                         (List.length solutions) min_distinct
           }
         else
           (List.iter (fun v -> do_verbose_log
                                  (Printf.sprintf "found %s" (to_string v)))
              solutions;
            { status = Pass; expect = Expected; details = "ok" })
       end
    | _ ->
       match generator !sample_size with
       | None -> iterate n (succ m) solutions failures
       | Some v ->
          let r = predicate v in
          match r.status with
          | Pass -> iterate (succ n) 0
                      (if List.mem v solutions then solutions
                       else v :: solutions)
                      failures
          | Undefined -> iterate n (succ m) solutions failures
          | Fail ->
             iterate (succ n) 0 solutions ((v, r.details) :: failures)
  in
  iterate 0 0 [] []

let quantified q dom df =
  let v = Oqc_descr.freshvar (df (Oqc_descr.atom "")) in
  let qd = Oqc_descr.prefix (100, q)
             (Oqc_descr.infix v (false, 50, " ∈ ") dom)
  in
  Oqc_descr.infix qd (false, 1000, ". ") (df v)

module Forall (P : PROPERTY) =
  struct
    module D = UnitDomain

    let predicate () = search_solution
                         ~to_string: (fun x -> Oqc_descr.to_string (P.D.describe x))
                         ~min_distinct: 0
                         ~max_distinct: max_int ~max_failures: 0
                         ~generator: P.D.arbitrary ~predicate: P.predicate

    let description _ = quantified "∀ " P.D.description P.description

  end

module Exists (P : PROPERTY) =
  struct
    module D = UnitDomain

    let predicate () = search_solution
                         ~to_string: (fun x -> Oqc_descr.to_string (P.D.describe x))
                         ~min_distinct: 1
                         ~max_distinct: max_int ~max_failures: max_int
                         ~generator: P.D.arbitrary ~predicate: P.predicate


    let description _ = quantified "∃ " P.D.description P.description
  end

module ExistsUnique (P : PROPERTY)  =
  struct
    module D = UnitDomain

    let predicate () = search_solution
                         ~to_string: (fun x -> Oqc_descr.to_string (P.D.describe x))
                         ~min_distinct: 1
                         ~max_distinct: 1 ~max_failures: max_int
                         ~generator: P.D.arbitrary ~predicate: P.predicate

    let description _ = quantified "∃! " P.D.description P.description
  end

module ExistDistinct (P : PROPERTY)  =
  struct
    module D = UnitDomain

    let predicate () = search_solution
                         ~to_string: (fun x -> Oqc_descr.to_string (P.D.describe x))
                         ~min_distinct: 2
                         ~max_distinct: max_int ~max_failures: max_int
                         ~generator: P.D.arbitrary ~predicate: P.predicate

    let description _ = quantified "∃₂ " P.D.description P.description
  end
