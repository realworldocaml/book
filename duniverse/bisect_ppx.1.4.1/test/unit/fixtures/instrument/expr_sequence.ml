let () =
  print_endline "abc"

let () =
  print_endline "abc";
  print_endline "def"

let () =
  print_endline "abc";
  print_endline "def";
  print_endline "ghi"

let () =
  begin
    print_endline "abc";
    function
    | 0 -> print_endline "def";
    | _ -> print_endline "ghi"
  end
  |> ignore

let () =
  let f ?maybe () = ignore maybe in
  () |> f
