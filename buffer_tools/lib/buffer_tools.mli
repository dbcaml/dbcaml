module Encode = Encode

(* Takes a function that returns bytes and calculate length of the bytes and then prepend the length to the bytes and return the new bytes:w
 *)
val put_length_prefixed : Buffer.t -> (Buffer.t -> unit) -> unit

val put_str_null : Buffer.t -> string -> unit
