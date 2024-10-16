module type Intf = sig
  type config

  val connect : config -> ('conn, string) result
end

type t =
  | Driver : {
      driver: (module Intf with type config = 'config);
      config: 'config;
    }
      -> t
