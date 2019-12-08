open Core
open Import
include Core.User_and_group

let for_this_process () =
  Unix.getlogin ()
  >>= fun user ->
  let gid = Unix.getgid () in
  Unix.Group.getbygid gid
  >>| function
  | None -> error "Can't find group" (`gid gid) [%sexp_of: [`gid of int]]
  | Some group -> Ok (create ~user ~group:group.name)
;;

let for_this_process_exn () = for_this_process () >>| Or_error.ok_exn
