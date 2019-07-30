
let { Lexing.pos_lnum = _; _} = [%here]  (* test that we've got pa_here loaded *)

let () =
  let failwiths ~here:{Lexing.pos_lnum = _; _} () = () in
  (* compilation fails if this is not rewritten to failwithp with the right arguments *)
  failwiths ();
;;
