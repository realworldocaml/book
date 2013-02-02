open Core.Std

module D = Dictionary

let () =
  let d = D.create () in
  D.add d ~key:3 ~data:"five";
  D.add d ~key:4 ~data:"four";
  assert (D.length d = 2);
  D.add d ~key:3 ~data:"three";
  assert (D.length d = 2);
  D.add d ~key:7 ~data:"seven";
  assert (D.length d = 3);
  D.remove d 4;
  assert (D.length d = 2);
  D.remove d 8;
  assert (D.length d = 2);
  let stack = Stack.create () in
  D.iter d ~f:(fun ~key ~data -> Stack.push stack (key,data));
  let l = List.sort ~cmp:compare (Stack.to_list stack) in
  assert (l = [3,"three";7,"seven"]);
  assert (D.find d 7 = Some "seven");
  assert (D.find d 4 = None);
  assert (D.find d 100 = None);
;;


let () = printf "tests passed\n"
