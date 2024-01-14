open Dbcaml

let () =
  print_endline "Starting up";

  Riot.run @@ fun () ->
  print_endline "Riot booted";

  let _ =
    PgPool.connect
      "host=localhost port=6437 user=postgres password=mysecretpassword dbname=development sslmode=disable"
  in

  Printf.printf "Hello world\n%!"
