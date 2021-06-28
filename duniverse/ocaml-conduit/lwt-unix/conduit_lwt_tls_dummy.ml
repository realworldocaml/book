module X509 = struct
  let private_of_pems ~cert:_ ~priv_key:_ =
    Lwt.fail_with "Tls not available"
end

module Client = struct
  let connect ?src:_ ?certificates:_ _host _sa =
    Lwt.fail_with "Tls not available"
end

module Server = struct
  let init' ?backlog:_ ?stop:_ ?timeout:_ _tls _sa _callback =
    Lwt.fail_with "Tls not available"

  let init ?backlog:_ ~certfile:_ ~keyfile:_ ?stop:_ ?timeout:_ _sa _callback =
    Lwt.fail_with "Tls not available"
end

let available = false
