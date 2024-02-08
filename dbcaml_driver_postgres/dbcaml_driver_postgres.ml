open Postgresql
open Dbcaml.Connection

let ( let* ) = Result.bind

let wait_for_result c =
  c#consume_input;
  while c#is_busy do
    ignore (Unix.select [Obj.magic c#socket] [] [] (-1.0));
    c#consume_input
  done

let fetch_result c =
  wait_for_result c;
  c#get_result

let fetch_single_result c =
  match fetch_result c with
  | None -> assert false
  | Some r ->
    assert (fetch_result c = None);
    r

module Postgres = struct
  type config = { conninfo: string }

  let connect config =
    let c = new connection ~conninfo:config.conninfo () in

    (*
     * Create the execute function that also use the PGOCaml.connection to send a request to Postgres database. 
     * This function is used by the Connection.make function to create a new connection
     *)
    let execute
        (conn : 'conn) (params : Dbcaml.Connection.execute_params array) query :
        (Dbcaml.Row.t list, string) Dbcaml.Errors.result =
      try
        conn#send_query ~params query;

        let result = fetch_single_result c in

        match result#status with
        | Command_ok
        | Tuples_ok ->
          let res = result#get_all_lst in

          let rows = List.map (fun x -> List.map unescape_bytea x) res in
          Ok rows
        | _ -> Error result#error
      with
      | Postgresql.Error e -> Error (string_of_error e)
      | e -> Error (Printexc.to_string e)
    in

    (* Create a new connection while we also want to use to create a PID *)
    let* conn = Dbcaml.Connection.make ~conn:c ~execute () in

    Ok conn
end

(*Create a new postgres driver using the module Postgress and the config provided *)
let connection conninfo =
  Dbcaml.Driver.Driver { driver = (module Postgres); config = { conninfo } }
