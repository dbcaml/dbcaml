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
    let u = Uri.of_string config.conninfo in
    let host = Uri.host u |> Option.value ~default:"localhost" in
    let port = Uri.port u |> Option.value ~default:5432 in
    let user = Uri.user u |> Option.value ~default:"postgres" in
    let password = Uri.password u |> Option.value ~default:"" in
    let database = Uri.path u |> remove_first_slash in

    print_endline "connecting";
    let c = PGOCaml.connect ~host ~port ~user ~password ~database () in

    print_endline "connected";
    PGOCaml.ping c;

    (*
     * Create the execute function that also use the PGOCaml.connection to send a request to Postgres database. 
     * This function is used by the Connection.make function to create a new connection
     *)
    let execute c query =
      let name = "dbcaml." ^ Digest.to_hex (Digest.string query) in
      PGOCaml.prepare c ~name ~query ();
      let row = PGOCaml.execute c ~name ~params:[] () in

      Ok row
    in

    (* Create a new connection while we also want to use to create a PID *)
    let* conn = Dbcaml.Connection.make ~conn:c ~execute () in

    Ok conn
end

(*Create a new postgres driver using the module Postgress and the config provided *)
let connection conninfo =
  Dbcaml.Driver.Driver { driver = (module Postgres); config = { conninfo } }