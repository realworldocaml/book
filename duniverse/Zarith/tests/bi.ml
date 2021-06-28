(* stress test, using random and corner cases 
   compares Big_int_Z, a Big_int compatible interface for Z, to OCaml's
   reference Big_int library

   This file is part of the Zarith library 
   http://forge.ocamlcore.org/projects/zarith .
   It is distributed under LGPL 2 licensing, with static linking exception.
   See the LICENSE file included in the distribution.

   Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
   Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
   a joint laboratory by:
   CNRS (Centre national de la recherche scientifique, France),
   ENS (École normale supérieure, Paris, France),
   INRIA Rocquencourt (Institut national de recherche en informatique, France).

*)


module B = Big_int (* reference library *)

module T = Big_int_Z  (* tested library *)


(* randomness *)

let _ = Random.init 42

let random_int64 () =
  let a,b,c = Random.bits(), Random.bits(), Random.bits () in
  let a,b,c = Int64.of_int a, Int64.of_int b, Int64.of_int c in
  let a,b,c = Int64.shift_left a 60, Int64.shift_left b 30, c in
  Int64.logor a (Int64.logor b c)

let random_int () = Int64.to_int (random_int64 ())

let random_string () =
  let l = 1 + Random.int 200 in
  let s = Buffer.create l in
  let st = if l > 1 && Random.bool () then begin
      Buffer.add_char s '-';
      1
    end else 0 in
  for i = st to l - 1 do
   Buffer.add_char s (Char.chr (48 + Random.int 10))
  done;
  Buffer.contents s


(* list utility *)

let list_make n f =
  let rec doit i acc = if i < 0 then acc else doit (i-1) ((f i)::acc) in
  doit (n-1) []


(* interesting numbers, as big_int *)

let p = (list_make 128 (B.shift_left_big_int B.unit_big_int))
let pn = p @ (List.map B.minus_big_int p)
let g_list = 
  [B.zero_big_int] @ 
  pn @ (List.map B.succ_big_int pn) @ (List.map B.pred_big_int pn) @
  (list_make 128 (fun _ -> B.big_int_of_int (random_int ()))) @
  (list_make 128 (fun _ -> B.big_int_of_string (random_string())))

let sh_list = list_make 256 (fun x -> x)
let pow_list = [1;2;3;4;5;6;7;8;9;10;20;55]

(* conversion to Z *)

let g_t_list =
  Printf.printf "converting %i numbers\n%!" (List.length g_list);
  List.map
    (fun g -> 
      let t = T.big_int_of_string (B.string_of_big_int g) in
      let g' = B.big_int_of_string (T.string_of_big_int t) in
      if B.compare_big_int g g' <> 0 then failwith (Printf.sprintf "string_of_big_int failure: %s" (B.string_of_big_int g));
      g, t
    )
    g_list

let rec cut_list n l =
  if n <= 0 then [] else match l with [] -> [] | h :: t -> h :: cut_list (n-1) t

let small_g_t_list = cut_list 256 g_t_list

(* operator tests *)

let test_un msg filt gf tf =
  Printf.printf "testing %s on %i numbers\n%!" msg (List.length g_t_list);
  List.iter
    (fun (g,t) ->
      try
        if filt g then (
          let g' = gf g and t' = tf t in
          if B.string_of_big_int g' <> T.string_of_big_int t' then failwith (Printf.sprintf "%s failure: arg=%s Bresult=%s Tresult=%s" msg (B.string_of_big_int g) (B.string_of_big_int g') (T.string_of_big_int t'))
         )
      with Failure _ -> ()
    ) g_t_list

let test_bin_gen msg filt gf tf l =
  Printf.printf "testing %s on %i x %i numbers\n%!" msg (List.length l) (List.length l);
  List.iter
    (fun (g1,t1) ->
      List.iter
        (fun (g2,t2) ->
          if filt (g1,g2) then (
            let g' = gf g1 g2 and t' = tf t1 t2 in
            if B.string_of_big_int g' <> T.string_of_big_int t' then failwith (Printf.sprintf "%s failure: arg1=%s arg2=%s Bresult=%s Tresult=%s" msg (B.string_of_big_int g1) (B.string_of_big_int g2) (B.string_of_big_int g') (T.string_of_big_int t'))
           )
        ) l
    ) l

let test_bin msg filt gf tf = test_bin_gen msg filt gf tf g_t_list
let test_bin_small msg filt gf tf = test_bin_gen msg filt gf tf small_g_t_list

let test_shift msg gf tf =
  Printf.printf "testing %s on %i numbers\n%!" msg (List.length g_t_list);
  List.iter
    (fun s ->
      List.iter
        (fun (g,t) ->
          let g' = gf g s and t' = tf t s in
          if B.string_of_big_int g' <> T.string_of_big_int t' then failwith (Printf.sprintf "%s failure: arg1=%s arg2=%i Bresult=%s Tresult=%s" msg (B.string_of_big_int g) s (B.string_of_big_int g') (T.string_of_big_int t'))
        ) g_t_list
    ) sh_list

let test_pow msg gf tf =
  Printf.printf "testing %s on %i numbers\n%!" msg (List.length g_t_list);
  List.iter
    (fun s ->
      List.iter
        (fun (g,t) ->
          let g' = gf g s and t' = tf t s in
          if B.string_of_big_int g' <> T.string_of_big_int t' then failwith (Printf.sprintf "%s failure: arg1=%s arg2=%i Bresult=%s Tresult=%s" msg (B.string_of_big_int g) s (B.string_of_big_int g') (T.string_of_big_int t'))
        ) g_t_list
    ) pow_list

let filt_none _ = true
let filt_pos x = B.sign_big_int x >= 0
let filt_nonzero2 (_,d) = B.sign_big_int d <> 0
let filt_pos2 (x,y) = B.sign_big_int x >= 0 && B.sign_big_int y >= 0
let filt_nonzero22 (x,y) = B.sign_big_int x <> 0 && B.sign_big_int y <> 0

let ffst f x = fst (f x)
let fsnd f x = snd (f x)
let ffst2 f x y = fst (f x y)
let fsnd2 f x y = snd (f x y)

let _ = test_un "int_of_big_int" filt_none (fun x -> x) (fun x -> T.big_int_of_int (T.int_of_big_int x))
let _ = test_un "int32_of_big_int" filt_none (fun x -> x) (fun x -> T.big_int_of_int32 (T.int32_of_big_int x))
let _ = test_un "int64_of_big_int" filt_none (fun x -> x) (fun x -> T.big_int_of_int64 (T.int64_of_big_int x))
let _ = test_un "nativeint_of_big_int" filt_none (fun x -> x) (fun x -> T.big_int_of_nativeint (T.nativeint_of_big_int x))
let _ = test_un "string_of_big_int" filt_none (fun x -> x) (fun x -> T.big_int_of_string (T.string_of_big_int x))

let _ = test_un "minus_big_int" filt_none B.minus_big_int T.minus_big_int
let _ = test_un "abs_big_int" filt_none B.abs_big_int T.abs_big_int
let _ = test_un "succ_big_int"filt_none  B.succ_big_int T.succ_big_int
let _ = test_un "pred_big_int" filt_none B.pred_big_int T.pred_big_int
let _ = test_un "sqrt_big_int" filt_pos B.sqrt_big_int T.sqrt_big_int

let _ = test_bin "add_big_int" filt_none B.add_big_int T.add_big_int
let _ = test_bin "sub_big_int" filt_none B.sub_big_int T.sub_big_int
let _ = test_bin "mult_big_int" filt_none B.mult_big_int T.mult_big_int
let _ = test_bin_small "div_big_int" filt_nonzero2 B.div_big_int T.div_big_int
let _ = test_bin_small "quomod_big_int #1" filt_nonzero2 (ffst2 B.quomod_big_int) (ffst2 T.quomod_big_int)
let _ = test_bin_small "quomod_big_int #2" filt_nonzero2 (fsnd2 B.quomod_big_int) (fsnd2 T.quomod_big_int)
let _ = test_bin_small "mod_big_int" filt_nonzero2 B.mod_big_int T.mod_big_int
let _ = test_bin_small "gcd_big_int" filt_nonzero22 B.gcd_big_int T.gcd_big_int

let _ = test_bin "and_big_int" filt_pos2 B.and_big_int T.and_big_int
let _ = test_bin "or_big_int" filt_pos2 B.or_big_int T.or_big_int
let _ = test_bin "xor_big_int" filt_pos2 B.xor_big_int T.xor_big_int

let _ = test_shift "shift_left_big_int" B.shift_left_big_int T.shift_left_big_int
let _ = test_shift "shift_right_big_int" B.shift_right_big_int T.shift_right_big_int
let _ = test_shift "shift_right_towards_zero_big_int" B.shift_right_towards_zero_big_int T.shift_right_towards_zero_big_int

let _ = test_pow "power_big_int_positive_int" B.power_big_int_positive_int T.power_big_int_positive_int

let _ = Printf.printf "All tests passed!\n"
