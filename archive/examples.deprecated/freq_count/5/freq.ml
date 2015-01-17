open Core.Std

module Make (Counter : Counter_intf.S) = struct

  let rec build_counts counts =
    match In_channel.input_line stdin with
    | None -> counts
    | Some line -> build_counts (Counter.touch counts line)

  let run () =
    let counts =
      List.sort ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
        (Counter.to_list (build_counts Counter.empty))
    in
    List.iter (List.take counts 10) ~f:(fun (line,count) ->
      printf "%3d: %s\n" count line)

end

module Fast = Make(Fast_counter)
module Slow = Make(Slow_counter)
let () =
  match Sys.getenv "FAST" with
  | None | Some "false" -> Slow.run ()
  | Some "true" -> Fast.run ()
  | Some x -> failwith ("Unexpected value of FAST env variable: " ^ x)
