let _ =
  match%lwt Lwt.return () with
  | exception (Invalid_argument _) -> Lwt.return 0
