let measure runs name f =
  let rec run = function
    | 0 -> ()
    | n -> f (); run (n - 1)
  in

  let start_time = Unix.gettimeofday () in

  run runs;

  let duration = (Unix.gettimeofday ()) -. start_time in
  let average = duration /. (float_of_int runs) *. 1000000. in

  Printf.printf "%s: %.0f us\n" name average

open Soup

let () =
  let html = read_file "../pages/google.html" in

  measure 100 "parse" (fun () -> parse html |> ignore);
  let soup = parse html in

  let traverse = fun () -> soup |> descendants |> count in
  assert (traverse () > 100);
  measure 1000 "traverse" (fun () -> traverse () |> ignore);

  let selector = "form[action*=search]" in
  assert (soup $ selector |> name = "form");
  measure 1000 "select" (fun () -> soup $ selector |> ignore);

  let selector = "*" in
  assert (soup $$ selector |> count > 10);
  measure 1000 "select_all" (fun () -> soup $$ selector |> count |> ignore)
