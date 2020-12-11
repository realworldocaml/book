let () =
  let p1 =
    let open Lwt.Let_syntax in
    let%bind x = Lwt.return 1 in
    let%map y = Lwt.return (x + 1) in
    y + 1
  in

  let p2 =
    let open Lwt_result.Let_syntax in
    let%bind x = Lwt_result.return 2 in
    let%map y = Lwt_result.return (x + 3) in
    x + y
  in

  let p =
    let%bind.Lwt p1 = p1 in
    let%map.Lwt_result p2 = p2 in
    p1 + p2
  in

  let x = Lwt_main.run p in

  if x = Ok 10 then
    exit 0
  else
    exit 1
