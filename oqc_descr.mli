type t

val to_string : t -> string

val atom : string -> t

val variable : string -> t

val prefix : int * string -> t -> t

val suffix : t -> int * string -> t

val wrap : string -> t -> string -> t

val infix : t -> bool * int * string -> t -> t

val freshvar : t -> t
