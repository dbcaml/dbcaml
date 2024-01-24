open Riot

module Logger = Logger.Make (struct
  let namespace = ["dbcaml"]
end)

let establish_single_connection conninfo =
  let u = Uri.of_string conninfo in

  let host = Uri.host u |> Option.value ~default:"localhost" in
  let port = Uri.port u |> Option.value ~default:5432 in
  let user = Uri.userinfo u |> Option.value ~default:"postgres" in
  let password = Uri.password u |> Option.value ~default:"" in
  let database = Uri.path u in

  let pid =
    spawn (fun () ->
        let c =
          Pgx_unix.connect
            ~host
            ~port
            ~user
            ~password
            ~database
            ~ssl:Pgx_unix.(`Auto)
            ~verbose:10
            ()
        in

        match receive () with
        | Types.Query query ->
          Logger.debug (fun f -> f "Got query: %s" query);

          let l = Pgx_unix.simple_query c query in
          (match l with
          | [] -> print_endline "empty"
          | _ ->
            print_endline "not empty";
            ()
          | _ -> failwith "unknown message"))
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
