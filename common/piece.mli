open! Core

type t = X | O [@@deriving sexp_of, equal, bin_io, enumerate]

val of_string : string -> t
val to_string : t -> string

(* [flip] gives you the "other" piece. | X -> O | O -> X *)
val flip : t -> t
val equal : t -> t -> bool
