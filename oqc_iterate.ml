open Oqc

let search_solution ~to_string ~min_distinct ~max_distinct
      ~max_failures ~generator ~predicate =
  let rec iterate n m solutions failures =
    if List.length failures > max_failures then
      Fail (String.concat ", " (List.map snd failures))
    else if List.length solutions > max_distinct then
      Fail (Printf.sprintf "%d solutions found, expected at most %d"
              (List.length solutions) max_distinct)
    else if m >= !max_attempts then
      Skip (Printf.sprintf "Arguments exhausted after %d tries" !max_attempts)
    else if n >= !min_iterations then
      (if List.length solutions < min_distinct then
         Skip (Printf.sprintf "%d solutions found, expected at least %d"
                 (List.length solutions) min_distinct)
       else
         (List.iter (fun v -> do_verbose_log
                                (Printf.sprintf "found %s" (to_string v)))
            solutions;
          Pass))
    else
      match generator !sample_size with
      | None -> iterate n (succ m) solutions failures
      | Some v ->
         match predicate v with
         | Pass -> (do_verbose_log
                     (Printf.sprintf "---> %s" (to_string v));
                      iterate (succ n) 0
                     (if List.mem v solutions then solutions else v :: solutions)
                     failures)
         | Skip _ -> iterate n (succ m) solutions failures
         | Fail msg | XFail msg ->
            iterate (succ n) 0 solutions ((v, msg) :: failures)
  in
  iterate 0 0 [] []

let quantified q dom df =
    let freshvar = Oqc_descr.variable ("x" ^
                                         string_of_int
                                           (List.length
                                              (Oqc_descr.variables (df (Oqc_descr.atom "")))))
    in
    let qd = Oqc_descr.prefix (100, q)
               (Oqc_descr.infix freshvar (false, 50, " ∈ ") dom)
    in
    Oqc_descr.infix qd (false, 1000, ". ") (df freshvar)

module Forall (P : PROPERTY) =
  struct
    module D = UnitDomain

    let predicate () = search_solution
                         ~to_string: (fun x -> Oqc_descr.to_string (P.D.to_descr x))
                         ~min_distinct: 0
                         ~max_distinct: max_int ~max_failures: 0
                         ~generator: P.D.arbitrary ~predicate: P.predicate

    let description _ = quantified "∀ " P.D.description P.description

  end

module Exists (P : PROPERTY) =
  struct
    module D = UnitDomain

    let predicate () = search_solution
                         ~to_string: (fun x -> Oqc_descr.to_string (P.D.to_descr x))
                         ~min_distinct: 1
                         ~max_distinct: max_int ~max_failures: max_int
                         ~generator: P.D.arbitrary ~predicate: P.predicate


    let description _ = quantified "∃ " P.D.description P.description
  end

module ExistsUnique (P : PROPERTY)  =
  struct
    module D = UnitDomain

    let predicate () = search_solution
                         ~to_string: (fun x -> Oqc_descr.to_string (P.D.to_descr x))
                         ~min_distinct: 1
                         ~max_distinct: 1 ~max_failures: max_int
                         ~generator: P.D.arbitrary ~predicate: P.predicate

    let description _ = quantified "∃! " P.D.description P.description
  end

module ExistDistinct (P : PROPERTY)  =
  struct
    module D = UnitDomain

    let predicate () = search_solution
                         ~to_string: (fun x -> Oqc_descr.to_string (P.D.to_descr x))
                         ~min_distinct: 2
                         ~max_distinct: max_int ~max_failures: max_int
                         ~generator: P.D.arbitrary ~predicate: P.predicate

    let description _ = quantified "∃₂ " P.D.description P.description
  end
