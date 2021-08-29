let from_unix : Unix.file_descr -> int = Obj.magic

class engine = object
  inherit Lwt_engine.abstract

  val loop = ref (Luv.Loop.default ())

  method! fork =
    Luv.Loop.fork !loop |> function
    | Ok () -> ()
    | Error e -> failwith (Printf.sprintf "Could not handle the fork, this is probably a error in Lwt, please open a issue on the repo. \nError message: %s" (Luv.Error.err_name e))

  method private cleanup = Luv.Loop.stop !loop

  method iter block =
    match (block) with
      | true -> Luv.Loop.run ~loop:!loop ~mode:`ONCE () |> ignore
      | false -> Luv.Loop.run ~loop:!loop ~mode:`NOWAIT () |> ignore

  method private register_readable fd f =
    let p = Luv.Poll.init ~loop:!loop (from_unix fd) in
    match p with
    | Ok poll ->
        let () = Luv.Poll.start poll [`READABLE;] (fun _ -> f ()) in
        lazy(
          Luv.Poll.stop poll |> function
          | Ok () -> ()
          | Error e -> failwith (Printf.sprintf "Could not stop read polling, this is probably a error in Lwt, please open a issue on the repo. \nError message: %s" (Luv.Error.err_name e))
        )
    | Result.Error e -> failwith (Printf.sprintf "Could not register fd for read polling, this is probably a error in Lwt, please open a issue on the repo. \nError message: %s" (Luv.Error.err_name e))

  method private register_writable fd f =
    let p = Luv.Poll.init ~loop:!loop (from_unix fd) in
    match p with
    | Ok poll ->
      let () = Luv.Poll.start poll [`WRITABLE;] (fun _ -> f ()) in
      lazy(
          Luv.Poll.stop poll |> function
          | Ok () -> ()
          | Error e -> failwith (Printf.sprintf "Could not stop write polling, this is probably a error in Lwt, please open a issue on the repo. \nError message: %s" (Luv.Error.err_name e))
        )
    | Result.Error e -> failwith (Printf.sprintf "Could not register fd for write polling, this is probably a error in Lwt, please open a issue on the repo. \nError message: %s" (Luv.Error.err_name e))

  method private register_timer delay repeat f =
    let delay_ms = (int_of_float (delay *. 1000.)) in
    let t = Luv.Timer.init ~loop:!loop () in
    match t with
    | Result.Error e -> failwith (Printf.sprintf "Could not initialize a timer, this is probably a error in Lwt, please open a issue on the repo. \nError message: %s" (Luv.Error.err_name e))
    | Ok timer ->
      let timer_fn = match repeat with
      | true -> Luv.Timer.start ~repeat:delay_ms timer
      | false -> Luv.Timer.start timer
      in
      match timer_fn delay_ms f with
      | Ok () -> lazy(Luv.Timer.stop timer |> ignore)
      | Result.Error e -> failwith (Printf.sprintf "Could not start a timer, this is probably a error in Lwt, please open a issue on the repo. \nError message: %s" (Luv.Error.err_name e))
end
