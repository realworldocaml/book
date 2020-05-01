
val close : 'a Lwt_io.channel * 'b Lwt_io.channel -> unit Lwt.t

val set_max_active : int -> unit

val listen : ?backlog:int -> Unix.sockaddr -> Lwt_unix.file_descr Lwt.t

val with_socket
  : Unix.sockaddr
  -> (Lwt_unix.file_descr -> 'a Lwt.t)
  -> 'a Lwt.t

val process_accept
  : ?timeout:int
  -> ('a -> 'b Lwt_io.channel -> 'c Lwt_io.channel -> unit Lwt.t)
  -> 'a * 'b Lwt_io.channel * 'c Lwt_io.channel
  -> unit Lwt.t

val init
  : ?stop:unit Lwt.t
  -> (Lwt_unix.file_descr * Lwt_unix.sockaddr -> unit Lwt.t)
  -> Lwt_unix.file_descr
  -> unit Lwt.t
