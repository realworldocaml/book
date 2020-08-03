module IO_run = struct
  type 'a t = 'a

  let return x = x
  let bind t ~f = f t
end

module IO_flush = struct
  include IO_run

  let to_run t = t
end

let flush () = () (* the runtime already flushes [stdout] *)

let run f = f ()
let flushed () = true (* the runtime flushed [stdout] before calling this function *)

let upon_unreleasable_issue = `CR
