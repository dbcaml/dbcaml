module type Intf = sig
  type config

  val connect : config -> (string, string) result
end

type t =
  | Connection : {
      driver: (module Intf with type config = 'config);
      config: 'config;
    }
      -> t
