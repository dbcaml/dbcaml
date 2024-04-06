let start ?(connections = 10) conninfo =
  let driver = Dbcaml_driver_postgres.connection conninfo in

  Dbcaml.start_link ~connections driver

let fetch_one ?(params = None) connection_manager_pid ~query =
  let result = Dbcaml.raw_execute connection_manager_pid params query in

  result
