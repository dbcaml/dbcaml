(** Create a interface which returns back a Dbcaml.Driver.t type. This type is used to create a connection and make queries *)
val connection : string -> Dbcaml.Driver.t
