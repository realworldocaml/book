open Core.Std

module type Reader = sig

  module Event : sig
    type t
    val of_string : string -> t Or_error.t
    val time : t -> Time.t option
  end

  module Report : sig
    type t
    val create : unit -> t
    val register_event : t -> Event.t -> unit
    val register_error : t -> Error.t -> unit
    val to_string : t -> string
  end
end

let process_log reader file =
  let module Reader = (val reader : Reader) in
  let open Reader in
  let report = Report.create () in
  In_channel.with_file file
    ~f:(In_channel.iter_lines ~f:(fun line ->
      match Event.of_string line with
      | Ok ev     -> Report.register_event report ev
      | Error err -> Report.register_error report err
    ));
  printf "LOG REPORT for file: %s\n" file;
  printf "%s\n" (Report.to_string report)
