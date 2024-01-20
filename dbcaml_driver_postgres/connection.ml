open Riot

let open_connection_to_server server_address port =
  let address =
    match server_address with
    | "localhost" -> Unix.inet_addr_loopback
    | _ -> Unix.inet_addr_of_string server_address
  in

  let socket_address = Unix.ADDR_INET (address, port) in
  Unix.open_connection socket_address

let connect connection_string =
  let u = Uri.of_string connection_string in
  let server_address =
    match Uri.host u with
    | Some x -> x
    | None -> failwith "No server address provided"
  in

  let port =
    match Uri.port u with
    | Some x -> x
    | None -> failwith "No port provided"
  in

  let c =
    try
      let (in_channel, out_channel) =
        open_connection_to_server server_address port
      in
      Authentication.send_startup_message out_channel;
      Authentication.handle_auth_request out_channel;

      Ok (in_channel, out_channel)
    with
    | e -> Error (Printexc.to_string e)
  in

  c

let send_message (in_channel, out_channel) message =
  try
    (* Send message to Postgres *)
    output_string out_channel (Printf.sprintf "%s\n" message);
    flush out_channel;

    let response = input_line in_channel in
    Logger.debug (fun f -> f "Response from server: %s " response);

    close_in in_channel;
    close_out out_channel
  with
  | e ->
    Logger.error (fun f -> f "An error occurred: %s " (Printexc.to_string e))
