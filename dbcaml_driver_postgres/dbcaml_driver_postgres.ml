open Postgresql

let ( let* ) = Result.bind

let remove_first_slash s =
  try
    let index = String.index s '/' in
    String.sub s 0 index ^ String.sub s (index + 1) (String.length s - index - 1)
  with
  | Not_found -> s (* Return the original string if no slash is found *)

module Postgres = struct
  type config = { conninfo: string }

  let connect config =
    let c = new connection ~conninfo:config.conninfo () in

    (*
     * Create the execute function that also use the PGOCaml.connection to send a request to Postgres database. 
     * This function is used by the Connection.make function to create a new connection
     *)
    let execute (_ : 'conn) params query :
        (* TODO: remove this type as it should be inherited*)
        (string option list list, [> `msg of string ]) Io.io_result =
      let result = c#exec ~expect:[Tuples_ok] ~params query in
      let rows =
        result#get_all_lst |> List.map (fun x -> List.map (fun r -> Some r) x)
      in

      Ok rows
    in

    (* Create a new connection while we also want to use to create a PID *)
    let* conn = Dbcaml.Connection.make ~conn:c ~execute () in

    Ok conn
end

(*Create a new postgres driver using the module Postgress and the config provided *)
let connection conninfo =
  Dbcaml.Driver.Driver { driver = (module Postgres); config = { conninfo } }
