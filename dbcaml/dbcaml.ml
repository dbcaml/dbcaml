module Connection = Connection
module Driver = Driver

module Dbcaml = struct
  let start_link (d : Driver.t) =
    match Driver.connect d with
    | Ok connection ->
      let rows = Connection.execute connection "select * from users" in

      List.iter
        (fun (x : Row.t) ->
          let _ = Row.map_to x in
          print_newline ())
        rows;

      ()
    | Error e -> print_endline e
end

(*
   TODO:
     1. Create a function that setups the connection and returns a connection object with PIDs
     2. Create a connection manager
*)
