open Core
open Poly
module Quota = Unix_extended.Quota

let id_kind_and_lookup = function
  | `User -> "user", fun name -> (Unix.Passwd.getbyname_exn name).Unix.Passwd.uid
  | `Group -> "group", fun name -> (Unix.Group.getbyname_exn name).Unix.Group.gid
;;

module Query = struct
  let make_named_command user_or_group =
    let id_kind, lookup_id = id_kind_and_lookup user_or_group in
    ( id_kind
    , Command.basic_spec
        ~summary:(sprintf "Query a %s's quota" id_kind)
        Command.Spec.(
          step (fun f v -> f ~id:(lookup_id v))
          +> anon (String.uppercase id_kind %: string)
          +> anon ("DEVICE" %: Filename.arg_type))
        (fun ~id device () ->
           let bytes_limit, bytes_usage, inodes_limit, inodes_usage =
             Or_error.ok_exn (Quota.query user_or_group ~id ~path:device)
           in
           printf "== Usage ==\n";
           printf
             "  - Bytes  : %s\n"
             (Int63.to_string (bytes_usage : Quota.bytes Quota.usage :> Int63.t));
           printf
             "  - Inodes : %s\n"
             (Int63.to_string (inodes_usage : Quota.inodes Quota.usage :> Int63.t));
           printf "== Limits ==\n";
           printf
             "  - Bytes  : %s\n"
             (Sexp.to_string ([%sexp_of: Quota.bytes Quota.limit] bytes_limit));
           printf
             "  - Inodes : %s\n"
             (Sexp.to_string ([%sexp_of: Quota.inodes Quota.limit] inodes_limit))) )
  ;;

  let named_command =
    ( "query"
    , Command.group
        ~summary:"Query quotas"
        [ make_named_command `User; make_named_command `Group ] )
  ;;
end

module Modify = struct
  let make_named_command user_or_group =
    let id_kind, lookup_id = id_kind_and_lookup user_or_group in
    ( id_kind
    , Command.basic_spec
        ~summary:(sprintf "Modify a %s's quota" id_kind)
        Command.Spec.(
          let make_nullable_arg_type ~zero parse =
            Arg_type.create (function
              | "" | "0" -> None
              | s when String.lowercase s = "none" -> None
              | s ->
                let x = parse s in
                if x = zero then None else Some x)
          in
          let bytes =
            make_nullable_arg_type ~zero:(Quota.bytes Int63.zero) (fun s ->
              s |> Byte_units.of_string |> Byte_units.bytes_int63 |> Quota.bytes)
          in
          let inodes =
            make_nullable_arg_type ~zero:(Quota.inodes Int63.zero) (fun s ->
              Quota.inodes (Int63.of_string s))
          in
          let grace =
            make_nullable_arg_type ~zero:Time.epoch (fun s ->
              try Time.of_string s with
              | exn ->
                (try Time.add (Time.now ()) (Time.Span.of_string s) with
                 | _ -> raise exn))
          in
          step (fun f v -> f ~id:(lookup_id v))
          +> anon (String.uppercase id_kind %: string)
          +> flag "-bytes-soft" (optional bytes) ~doc:"byte usage soft limit"
          +> flag "-bytes-hard" (optional bytes) ~doc:"byte usage hard limit"
          +> flag "-bytes-grace" (optional grace) ~doc:"byte usage grace period"
          +> flag "-inodes-soft" (optional inodes) ~doc:"inode usage soft limit"
          +> flag "-inodes-hard" (optional inodes) ~doc:"inode usage hard limit"
          +> flag "-inodes-grace" (optional grace) ~doc:"inode usage grace period"
          +> anon ("DEVICE" %: Filename.arg_type))
        (fun ~id bsoft bhard bgrace isoft ihard igrace device () ->
           let bytes_limit, _bytes_usage, inodes_limit, _inodes_usage =
             Or_error.ok_exn (Quota.query user_or_group ~id ~path:device)
           in
           let update_limit limit soft hard grace =
             let optional_update field update =
               match field with
               | None -> Fn.id
               | Some v -> fun l -> update l v
             in
             List.fold
               ~init:limit
               ~f:(fun acc update -> update acc)
               [ optional_update soft (fun acc soft -> { acc with Quota.soft })
               ; optional_update hard (fun acc hard -> { acc with Quota.hard })
               ; optional_update grace (fun acc grace -> { acc with Quota.grace })
               ]
           in
           let bytes_limit = update_limit bytes_limit bsoft bhard bgrace in
           let inodes_limit = update_limit inodes_limit isoft ihard igrace in
           Or_error.ok_exn
             (Quota.set user_or_group ~id ~path:device bytes_limit inodes_limit)) )
  ;;

  let named_command =
    ( "modify"
    , Command.group
        ~summary:"Modify quotas"
        [ make_named_command `User; make_named_command `Group ] )
  ;;
end

let command =
  Command.group ~summary:"Set/query quotas" [ Query.named_command; Modify.named_command ]
;;

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
