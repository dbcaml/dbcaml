type t = C : { conn: 'conn } -> t

let make ~conn () = Ok (C { conn })
