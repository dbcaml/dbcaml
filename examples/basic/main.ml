open Riot
open Dbcaml

let () =
  Riot.run (fun () ->
      Logger.set_log_level (Some Debug);
      let _ =
        PgPool.connect
          "host=localhost port=6437 user=postgres password=mysecretpassword dbname=development sslmode=disable"
      in

      print_string "Hello world\n")
