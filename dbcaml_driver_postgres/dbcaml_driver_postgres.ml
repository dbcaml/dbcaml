open Riot

module Logger = Logger.Make (struct
  let namespace = ["dbcaml"]
end)

let establish_single_connection conninfo =
  let u = Uri.of_string conninfo in

  let host = Uri.host u |> Option.value ~default:"127.0.0.1" in
  let port = Uri.port u |> Option.value ~default:5432 in
  let user = Uri.user u |> Option.value ~default:"postgres" in
  let password = Uri.password u |> Option.value ~default:"" in
  let database = String.sub (Uri.path u) 1 (String.length (Uri.path u) - 1) in

  print_endline database;
  print_endline password;
  print_endline user;
  print_endline host;
  print_endline (string_of_int port);

  let pid =
    spawn (fun () ->
        Pgx_async.with_conn
          ~host
          ~port
          ~user
          ~password
          ~database
          ~ssl:`No
          ~verbose:10
          ()
        @@ fun db ->
        match receive () with
        | Types.Query query ->
          Logger.debug (fun f -> f "Got query: %s" query);
          Pgx_async.simple_query db query;

          ()
        | _ -> failwith "unknown message");

    ()
  in

  pid

module Postgres = struct
  type t = { max_connections: int }

  let default = { max_connections = 10 }

  let connect ?(max_connections = 10) conninfo =
    let connection_manager_pid =
      spawn (fun () ->
          let connections =
            Array.make
              max_connections
              (try establish_single_connection conninfo with
              | e -> failwith (Printexc.to_string e))
            |> Array.to_list
          in

          Logger.debug (fun f ->
              f "Created %d connections" (List.length connections));
          match receive () with
          | Types.Query query ->
            let c = List.hd connections in
            (* FIXME: select a not occupied connection *)
            Logger.debug (fun f -> f "Sending query to connection: %a" Pid.pp c);
            send c (Types.Query query)
          | Types.ReadyStatus status ->
            Logger.debug (fun f -> f "Got status: %s" status)
          | _ -> failwith "unknown message")
    in

    Logger.debug (fun f ->
        f "Booting Connection manager with PID: %a" Pid.pp (self ()));

    send connection_manager_pid (Types.Query "SELECT * FROM users")
end
