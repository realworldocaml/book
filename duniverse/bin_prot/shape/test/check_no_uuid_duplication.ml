open Core
open Poly
open Async

type uuid_in_file =
  { fname : string
  ; line : int
  ; uuid : string
  }

let main () =
  let argv = Sys.get_argv () in
  assert (Array.length argv = 2);
  let%bind () = Unix.chdir argv.(1) in
  let%bind files =
    Process.run_lines_exn () ~prog:"hg" ~args:[ "stat"; "-numac" ]
    >>| List.filter ~f:(fun fn ->
      (not (String.is_prefix fn ~prefix:"external/"))
      && (* Filter out symlinks and other things. Use core as it would be too slow with
            async. *)
      (Core.Unix.lstat fn).st_kind = S_REG)
  in
  let%bind uuids =
    (* Break the list into chunks of 200 to stay under the command line length
       restrictions *)
    List.chunks_of files ~length:200
    |> Deferred.List.concat_map ~f:(fun files ->
      Process.run_lines_exn
        ()
        ~accept_nonzero_exit:[ 1 ]
        ~prog:"grep"
        ~args:("-HEno" :: "\"[a-f0-9]{8}-([a-f0-9]{4}-){3}[a-f0-9]{12}\"" :: files))
    >>| List.map ~f:(fun line ->
      Scanf.sscanf line {|%[^:]:%u:"%[^"]"|} (fun fname line uuid ->
        { fname; line; uuid }))
  in
  let dups =
    List.map uuids ~f:(fun u -> u.uuid, u)
    |> String.Map.of_alist_multi
    |> Map.filter ~f:(function
      | [] | [ _ ] -> false
      | _ :: _ :: _ -> true)
    |> Map.to_alist
  in
  match dups with
  | [] -> Shutdown.exit 0
  | l ->
    eprintf "Duplicated UUIDS found in the tree!\n";
    List.iter l ~f:(fun (uuid, occurences) ->
      eprintf "UUID %S appears in:\n" uuid;
      List.iter occurences ~f:(fun u -> eprintf "- %s:%u\n" u.fname u.line));
    Shutdown.exit 1
;;

let (_ : never_returns) =
  don't_wait_for (return () >>= main);
  Scheduler.go ()
;;
