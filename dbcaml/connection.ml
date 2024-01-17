open Unix

module Default = struct
  let open_connection_to_server server_address port =
    let inet_addr = inet_addr_of_string server_address in
    let socket_address = ADDR_INET (inet_addr, port) in
    open_connection socket_address

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

    try
      let (in_channel, out_channel) =
        open_connection_to_server server_address port
      in
      output_string out_channel "Hello, server!\n";
      flush out_channel;

      let response = input_line in_channel in
      Printf.printf "Response from server: %s\n" response;

      close_in in_channel;
      close_out out_channel
    with
    | e -> Printf.eprintf "An error occurred: %s\n" (Printexc.to_string e)
end
