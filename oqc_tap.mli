type status = Ok | Not_ok
type directive = Todo of string
               | Skip of string

val plan : int -> unit
val test : ?ord: int -> ?description: string -> ?directive: directive -> status -> unit
val comment : string -> unit
val bailout : string -> unit
