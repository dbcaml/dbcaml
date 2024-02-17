open Riot

open Logger.Make (struct
  let namespace = ["poolparty"; "storage"]
end)

type status =
  | Ready
  | Buzy

let change global_storage storage_mutex key value =
  debug (fun f -> f "changeing value for pid: %a" Pid.pp key);
  Mutex.lock storage_mutex;
  Hashtbl.replace global_storage key value;
  Mutex.unlock storage_mutex

let get global_storage storage_mutex key =
  Mutex.lock storage_mutex;
  let value = Hashtbl.find_opt global_storage key in
  Mutex.unlock storage_mutex;
  value

let remove global_storage storage_mutex key =
  Mutex.lock storage_mutex;
  Hashtbl.remove global_storage key;
  Mutex.unlock storage_mutex

let rec available_holder global_storage storage_mutex =
  Mutex.lock storage_mutex;
  let result =
    Hashtbl.fold
      (fun key x accu ->
        match x with
        | Ready -> (x, key) :: accu
        | _ -> accu)
      global_storage
      []
  in
  Mutex.unlock storage_mutex;

  try List.hd result with
  | _ ->
    debug (fun f -> f "not available holder, retrying...");
    sleep 0.100;
    available_holder global_storage storage_mutex
