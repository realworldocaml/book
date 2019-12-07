module M = struct
  type t = Foo | Bar
  type r = { i : int; t : t }
end

let f () =
  let s = M.Bar in
  (match s with | M.(Foo | Bar) -> assert true);
  let l = M.[Foo] in
  (match l with | M.[ Foo | Bar] -> assert true | _ -> assert false);
  let a = M.[|Bar|] in
  (match a with | M.[| Foo | Bar |] -> assert true | _ -> assert false);
  let r = M.{i=3; t = Foo} in
  (match r with | M.{i = 3 | 4 ; _} -> assert true | _ -> assert false);
