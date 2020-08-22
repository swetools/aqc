type outcome = Pass
             | Fail of string
             | Skip of string
             | XFail of string

exception Bailout of string
exception Exhausted of int * string

module type DOMAIN =
  sig
    type t

    val arbitrary : int -> t
    val to_string : t -> string
    val description : string
  end

module type PROPOSITION =
  sig
    val check : unit -> outcome
    val description : string
  end

module type PREDICATE =
  sig
    module D : DOMAIN
    val predicate : D.t -> outcome
    val description : string -> string
  end

module type RELATION =
  sig
    module D1 : DOMAIN
    module D2 : DOMAIN

    val relation : D1.t -> D2.t -> outcome
    val description : string -> string -> string
  end

let all_propositions : (module PROPOSITION) list ref = ref [] ;;

let register (m : (module PROPOSITION)) =
  all_propositions := m :: !all_propositions ;;

let do_log msg = Oqc_tap.comment msg

let run () =
  Oqc_tap.plan (List.length !all_propositions);
  try
    List.iteri (fun i m ->
                let module Prop = (val m : PROPOSITION) in
                match Prop.check () with
                | Pass -> Oqc_tap.test ~ord: i
                                       ~description: Prop.description
                                       (Ok ())
                | Fail msg -> Oqc_tap.test ~ord: i
                                           ~description: (Prop.description ^ ": " ^ msg)
                                           (Error ())
                | Skip msg -> Oqc_tap.test ~ord: i
                                           ~description: Prop.description
                                           ~directive: (Oqc_tap.Skip msg)
                                           (Ok ())
                | XFail msg -> Oqc_tap.test ~ord: i
                                            ~description: Prop.description
                                            ~directive: (Oqc_tap.Todo msg)
                                            (Error ())
                | exception Exhausted (n, msg) ->
                            Oqc_tap.test ~ord: i
                                         ~description: Prop.description
                                         ~directive: (Oqc_tap.Skip (msg ^ ": arguments exhausted after " ^ string_of_int n ^ " tries"))
                                         (Ok ()))
               !all_propositions
  with
    Bailout msg -> Oqc_tap.bailout msg

let min_iterations = ref 100

let max_attempts = ref 1000

let sample_size = ref 10
