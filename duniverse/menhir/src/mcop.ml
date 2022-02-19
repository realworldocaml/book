(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

type problem = int array

type 'a solution =
  | Matrix of 'a
  | Product of 'a solution * 'a solution

let rec map_solution f = function
  | Matrix x -> Matrix (f x)
  | Product (t, u) ->
    let t' = map_solution f t in
    let u' = map_solution f u in
    Product (t', u')

exception Empty

(** Try to handle trivial cases before falling back to smart algorithms. *)
let trivial_cases = function
  | [||] | [|_|] -> raise Empty
  (* One or two matrices, one solution *)
  | [|_; _|] -> Some (Matrix 0)
  | [|_; _; _|] -> Some (Product (Matrix 0, Matrix 1))
  | [|a; b; c; d|] ->
    (* Three matrices, two possible solutions.
       One test is sufficient. *)
    let cost_AB_C = a * b * c + a * c * d in
    let cost_A_BC = a * b * d + b * c * d in
    if cost_AB_C < cost_A_BC
    then Some (Product (Product (Matrix 0, Matrix 1), Matrix 2))
    else Some (Product (Matrix 0, Product (Matrix 1, Matrix 2)))
  | _ -> None

(* Left-leaning tree *)

let left_solution arr =
  let len = Array.length arr in
  if len <= 1 then raise Empty;
  let solution = ref (Matrix 0) in
  for i = 1 to len - 1 do
    solution := Product (!solution, Matrix i)
  done;
  !solution

(* Right-leaning tree *)

let right_solution arr =
  let len = Array.length arr in
  if len <= 1 then raise Empty;
  let solution = ref (Matrix (len - 1)) in
  for i = len - 2 downto 0 do
    solution := Product (Matrix i, !solution)
  done;
  !solution

(* Francis Chin O(n) approximation *)

(** Returns the index of the biggest float in the array *)
let argmax (a : float array) : int =
  let i = ref 0 in
  let m = ref a.(0) in
  for j = 1 to Array.length a - 1 do
    let m' = a.(j) in
    if m' > !m then begin
      i := j;
      m := m';
    end
  done;
  !i

(** Returns the permutation vector corresponding to problem [k] *)
let approx_vector (k : problem) =
  (* k.(0..n) *)
  let n = Array.length k - 1 in
  (* v.(0..n-2) *)
  let v = Array.make (n - 1) 0 in
  (* r.(0..n) *)
  let r = Array.map (fun k_i -> 1.0 /. float k_i) k in
  (* 0 <= m <= n, r_m = r.(m) *)
  let m = argmax r in
  let r_m = r.(m) in
  let c = ref 1 in
  let j = ref 0 in
  let s = Array.make (n + 1) 0 in
  (*Printf.printf "n=%d\n" n;*)
  for i = 1 to n - 1 do
    incr j;
    s.(!j) <- i;
    while !j > 0 && r.(s.(!j)) +. r_m < r.(s.(!j-1)) +. r.(i+1) do
      (*Printf.printf "j=%d s(j)=%d\n" !j s.(!j);*)
      v.(s.(!j)-1) <- !c;
      incr c;
      decr j;
    done;
  done;
  let b = ref (n - 1) in
  incr j;
  (*Printf.printf "j=%d " !j;*)
  (*Printf.printf "s(j)=%d\n" s.(!j);*)
  s.(!j) <- n;
  let k = ref 0 in
  let stop = ref false in
  while not !stop do
    if r.(s.(!k)) < r.(s.(!j)) then
      begin
        if r.(s.(!j)) +. r_m < r.(s.(!j-1)) +. r.(s.(!k)) then
          begin decr j; v.(s.(!j) - 1) <- !b; decr b; end
        else if r.(s.(!k)) +. r_m < r.(s.(!k+1)) +. r.(s.(!j)) then
          begin incr k;
            (*Printf.printf "k=%d " !k;*)
            (*Printf.printf "s(k)=%d " s.(!k);*)
            (*Printf.printf "b=%d\n" !b;*)
            if s.(!k) < n then v.(s.(!k)-1) <- !b;
            decr b;
          end
        else stop := true
      end
    else stop := true
  done;
  for i = m - 1 downto s.(!k) + 1
  do if v.(i-1) = 0 then begin v.(i-1) <- !c; incr c end done;
  for i = m + 1 to s.(!j) - 1
  do if v.(i-1) = 0 then begin v.(i-1) <- !c; incr c end done;
  if m > 0 && m < n && v.(m-1) = 0 then v.(m-1) <- !c;
  v

(* Parse the permutation vector produced by the approximation using
   a variant of shunting-yard algorithm *)

let approx_parse_vector order =
  let rec pop (n : int) x = function
    | (n', x') :: xs when n' <= n -> pop n (Product (x', x)) xs
    | other -> (n, x) :: other
  in
  let push (i, stack) v = (i + 1, pop v (Matrix i) stack) in
  let close (i, stack) = snd (List.hd (pop max_int (Matrix i) stack)) in
  let final_stack = Array.fold_left push (0, []) order in
  close final_stack

let approx_solution dims =
  match trivial_cases dims with
  | Some solution -> solution
  | None -> approx_parse_vector (approx_vector dims)

(* Optimal solution, O(n^3) using dynamic programming *)

(** Returns the solution matrix for problem [dims] *)
let dynamic_matrix (dims : problem) =
  let n = Array.length dims - 1 in
  let m = Array.make_matrix n n 0 in
  let s = Array.make_matrix n n 0 in
  for len = 1 to n - 1 do
    for i = 0 to n - len - 1 do
      let j = i + len in
      m.(i).(j) <- max_int;
      for k = i to j - 1 do
        let cost = m.(i).(k) + m.(k+1).(j) + dims.(i)*dims.(k+1)*dims.(j+1) in
        if cost < m.(i).(j) then begin
          m.(i).(j) <- cost;
          s.(i).(j) <- k;
        end
      done
    done
  done;
  s

let dynamic_parse_matrix s =
  let rec loop i j =
    if i <> j then (
      let i_s = loop i s.(i).(j) in
      let s_j = loop (s.(i).(j) + 1) j in
      Product (i_s, s_j)
    ) else
      Matrix i
  in
  loop 0 (Array.length s - 1)

let dynamic_solution dims =
  match trivial_cases dims with
  | Some solution -> solution
  | None -> dynamic_parse_matrix (dynamic_matrix dims)
