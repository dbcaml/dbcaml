(*
* Create a new type t that holds a connection and a function to execute a query which all the drivers will implement and use.
* This makes us able to  
*)

type param =
  | String of string
  | Number of int
  | Float of float
  | Bool of bool
  | Null

type t =
  | C : {
      (* 'conn is a generic *)
      conn: 'conn;
      (* This function takes a 'generic conn and a query. And return a Row.T list which is our type of a row *)
      execute:
        'conn -> param list -> string -> (bytes, Res.execution_error) Res.result;
    }
      -> t

(* Create a new connection based of type t.
   This function is used by the drivers to make it possible to have different drivers without doing a lot of matching what type the driver is *)
let make ~conn ~execute () = Ok (C { conn; execute })

let execute conn params query =
  match conn with
  | C c -> c.execute c.conn params query
