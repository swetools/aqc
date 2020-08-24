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

module ExpectFailure (P : PROPOSITION) =
  struct
    let check () =
      match P.check () with
      | Pass -> Pass
      | Skip _ as r -> r
      | Fail msg | XFail msg -> XFail msg

    let description = P.description
  end

module ExpectSkipped (P : PROPOSITION) =
  struct
    let check () =
      match P.check () with
      | Skip _ as r -> r
      | Fail msg | XFail msg -> Fail msg
      | Pass -> Fail "should be skipped"

    let description = P.description
  end

module Required (P : PROPOSITION) =
  struct
    let check () =
      match P.check () with
      | Skip msg | XFail msg | Fail msg -> Fail msg
      | Pass -> Pass

    let description = P.description
  end

let ensure ?message b =
  if b then Pass
  else
    Fail (match message with
          | Some msg -> msg
          | None -> String.map (function | '\n' -> ' '
                                         | x -> x)
                      (Printexc.raw_backtrace_to_string
                      (Printexc.get_callstack 2)))

let all_propositions : (module PROPOSITION) Queue.t = Queue.create () ;;

let register (m : (module PROPOSITION)) =
  Queue.add m all_propositions ;;

let do_log msg = Oqc_tap.comment msg

let run () =
  Oqc_tap.plan (Queue.length all_propositions);
  try
    for i = 1 to Queue.length all_propositions do
      let m = Queue.take all_propositions in
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
           (Ok ())
    done
  with
    Bailout msg -> Oqc_tap.bailout msg

let min_iterations = ref 100

let max_attempts = ref 1000

let sample_size = ref 10

let verbose = ref false

let do_verbose_log msg = if !verbose then do_log msg else ()
