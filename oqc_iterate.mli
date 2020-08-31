open Oqc

val search_solution : to_string: ('a -> string) ->
                      min_distinct: int -> max_distinct: int ->
                      max_failures: int ->
                      generator: (int -> 'a option) ->
                      predicate: ('a -> outcome) ->
                      outcome

module Forall (P : PROPERTY) : ATOMIC_PROPERTY
module Exists (P : PROPERTY) : ATOMIC_PROPERTY
module ExistsUnique (P : PROPERTY) : ATOMIC_PROPERTY
module ExistDistinct (P : PROPERTY) : ATOMIC_PROPERTY
