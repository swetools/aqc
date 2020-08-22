type result = Pass
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
    val check : unit -> result
    val description : string
  end
    
module type PREDICATE =
  sig
    module D : DOMAIN

    val predicate : D.t -> result
    val description : string -> string
  end
    
module type RELATION =
  sig
    module D1 : DOMAIN
    module D2 : DOMAIN

    val relation : D1.t -> D2.t -> result
    val description : string -> string -> string
  end

val register : (module PROPOSITION) -> unit

val do_log : string -> unit
                                         
val run : unit -> unit

val min_iterations : int ref

val max_attempts : int ref

val sample_size : int ref
                          
