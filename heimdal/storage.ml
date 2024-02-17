type status =
  | Ready
  | Buzy

let add global_storage storage_mutex key value =
  print_endline "Added item to storage";
  Mutex.lock storage_mutex;
  Hashtbl.add global_storage key value;
  Mutex.unlock storage_mutex

let change global_storage storage_mutex key value =
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
