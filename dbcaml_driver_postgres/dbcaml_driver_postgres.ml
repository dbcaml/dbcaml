open Postgresql

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
    let execute (conn : connection) (params : string array) (query : string) :
        (Dbcaml.Row.t list, 'b) Io.io_result =
      try
        let stmt_name =
          Digest.to_hex (Printf.sprintf "Dbcaml.%s" (Digest.to_hex query))
        in
        let param_types =
          Array.make (Array.length params) (oid_of_ftype INT8)
        in
        ignore (conn#prepare ~param_types stmt_name query);

        let result = conn#exec_prepared ~params stmt_name in

        match result#status with
        | Command_ok ->
          let res = result#get_all_lst in

          let rows = List.map (fun x -> List.map unescape_bytea x) res in
          Ok rows
        | _ -> Error `Io_error
      with
      | Postgresql.Error e -> Error e
      | e ->
        print_endline (Printexc.to_string e);
        Error `Io_error
    in

    (* Create a new connection while we also want to use to create a PID *)
    let* conn = Dbcaml.Connection.make ~conn:c ~execute () in

    Ok conn
end

(*Create a new postgres driver using the module Postgress and the config provided *)
let connection conninfo =
  Dbcaml.Driver.Driver { driver = (module Postgres); config = { conninfo } }
