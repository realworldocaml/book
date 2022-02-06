(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let single = function
  | [ x ] ->
      x
  | _ ->
      assert false

let rec mapd f = function
  | [] ->
      []
  | x :: xs ->
      let y1, y2 = f x in
      y1 :: y2 :: mapd f xs

let tabulate n f =
  let a = Array.init n f in
  Array.get a

let sum n (f : int -> int) : int =
  let sum = ref 0 in
  for x = 0 to n - 1 do
    sum := !sum + f x
  done;
  !sum

let with_buffer n f =
  let b = Buffer.create n in
  f b;
  Buffer.contents b

type 'a iter = ('a -> unit) -> unit

let separated_iter_to_string printer separator iter =
  with_buffer 32 (fun b ->
    let first = ref true in
    iter (fun x ->
      if !first then begin
        Buffer.add_string b (printer x);
        first := false
      end
      else begin
        Buffer.add_string b separator;
        Buffer.add_string b (printer x)
      end
    )
  )

let separated_list_to_string printer separator xs =
  separated_iter_to_string printer separator (fun f -> List.iter f xs)

let inverse (a : 'a array) : 'a -> int =
  let table = Hashtbl.create (Array.length a) in
  Array.iteri (fun i data ->
    assert (not (Hashtbl.mem table data));
    Hashtbl.add table data i
  ) a;
  fun data ->
    try
      Hashtbl.find table data
    with Not_found ->
      assert false

let support_assoc l x =
  try
    List.assoc x l
  with Not_found -> x

(* Turning an implicit list, stored using pointers through a hash
   table, into an explicit list. The head of the implicit list is
   not included in the explicit list. *)

let materialize (table : ('a, 'a option) Hashtbl.t) (x : 'a) : 'a list =
  let rec loop x =
    match Hashtbl.find table x with
    | None ->
        []
    | Some x ->
        x :: loop x
  in
  loop x

(* [iteri] implements a [for] loop over integers, from 0 to
   [n-1]. *)

let iterij i j f =
  for x = i to j - 1 do
    f x
  done

let iteri n f =
  iterij 0 n f

let rec foldij i j f accu =
  if i < j then
    foldij (i + 1) j f (f i accu)
  else
    accu

let foldi n f accu =
  foldij 0 n f accu

let rec foldij_lazy i j f accu =
  if i < j then
    f i (fun () -> foldij_lazy (i + 1) j f accu)
  else
    accu

(* [mapij start n f] produces the list [ f start; ... f (n-1) ]. *)

let mapij start n f =
  List.rev (
    foldij start n (fun i accu ->
      f i :: accu
    ) []
  )

(* [mapi n f] produces the list [ f 0; ... f (n-1) ]. *)

let mapi n f =
  mapij 0 n f

(* [qfold f accu q] repeatedly takes an element [x] off the queue [q]
   and applies [f] to the accumulator and to [x], until [q] becomes
   empty. Of course, [f] can add elements to [q] as a side-effect.

   We allocate an option to ensure that [qfold] is tail-recursive. *)

let rec qfold f accu q =
  match
    try
      Some (Queue.take q)
    with Queue.Empty ->
      None
  with
  | Some x ->
      qfold f (f accu x) q
  | None ->
      accu

(* [qiter f q] repeatedly takes an element [x] off the queue [q] and
   applies [f] to [x], until [q] becomes empty. Of course, [f] can add
   elements to [q] as a side-effect. *)

let qiter f q =
  try
    while true do
      f (Queue.take q)
    done
  with Queue.Empty ->
    ()

let rec smap f = function
  | [] ->
      []
  | (x :: xs) as l ->
      let x' = f x
      and xs' = smap f xs in
      if x == x' && xs == xs' then
        l
      else
        x' :: xs'

let rec smapa f accu = function
  | [] ->
      accu, []
  | (x :: xs) as l ->
      let accu, x' = f accu x in
      let accu, xs' = smapa f accu xs in
      accu,
      if x == x' && xs == xs' then
        l
      else
        x' :: xs'

let normalize s =
  let s = Bytes.of_string s in
  let n = Bytes.length s in
  for i = 0 to n - 1 do
    match Bytes.get s i with
    | '('
    | ')'
    | ',' ->
        Bytes.set s i '_'
    | _ ->
        ()
  done;
  Bytes.unsafe_to_string s

(* [postincrement r] increments [r] and returns its original value. *)

let postincrement r =
  let x = !r in
  r := x + 1;
  x

let mkgensym () =
  let r = ref 0 in
  fun () ->
    postincrement r

(* [filter_map f l] returns the list of [y]s such that [f x = Some y] where [x]
   is in [l], preserving the order of elements of [l]. *)
let filter_map f l =
  List.(rev (fold_left (fun ys x ->
    match f x with
      | None -> ys
      | Some y -> y :: ys
  ) [] l))

let new_encode_decode capacity =
  (* Set up a a hash table, mapping strings to unique integers. *)
  let module H = Hashtbl.Make(struct
    type t = string
    let equal = (=)
    let hash = Hashtbl.hash
  end) in
  let table = H.create capacity in
  (* Set up a resizable array, mapping integers to strings. *)
  let text = MenhirLib.InfiniteArray.make "" in
  (* This counts the calls to [encode]. *)
  let c = ref 0 in
  (* A string is mapped to a unique integer, as follows. *)
  let encode (s : string) : int =
    c := !c + 1;
    try
      H.find table s
    with Not_found ->
      (* The number of elements in the hash table is the next available
         unique integer code. *)
      let i = H.length table in
      H.add table s i;
      MenhirLib.InfiniteArray.set text i s;
      i
  (* An integer code can be mapped back to a string, as follows. *)
  and decode (i : int) : string =
    MenhirLib.InfiniteArray.get text i
  and verbose () =
    Printf.fprintf stderr
      "%d calls to intern; %d unique strings.\n%!"
      !c (H.length table)
  in
  encode, decode, verbose

let rec best (preferable : 'a -> 'a -> bool) (xs : 'a list) : 'a option =
  match xs with
  | [] ->
      (* Special case: no elements at all, so no best element. This case
         does not participate in the recursion. *)
      None
  | [x] ->
      Some x
  | x :: xs ->
      (* If [x] is preferable to every element of [xs], then it is the
         best element of [x :: xs]. *)
      if List.for_all (preferable x) xs then
        Some x
      else
        (* [xs] is nonempty, so the recursive call is permitted. *)
        match best preferable xs with
        | Some y ->
            if preferable y x then
              (* If [y] is the best element of [xs] and [y] is preferable to
                 [x], then [y] is the best element of [x :: xs]. *)
              Some y
            else
              (* There is no best element. *)
              None
        | None ->
            (* There is no best element. *)
            None

let rec levels1 cmp x1 xs =
  match xs with
  | [] ->
      [x1], []
  | x2 :: xs ->
      let ys1, yss = levels1 cmp x2 xs in
      if cmp x1 x2 = 0 then
        x1 :: ys1, yss
      else
        [x1], ys1 :: yss

let levels cmp xs =
  match xs with
  | [] ->
      []
  | x1 :: xs ->
      let ys1, yss = levels1 cmp x1 xs in
      ys1 :: yss

(* Suppose [ys] is a list of elements that are pairwise incomparable
   with respect to the partial order [<=], and [x] is a new element.
   Then, [insert (<=) x ys] is the list obtained by inserting [x] and
   removing any non-maximal elements; so it is again a list of pairwise
   incomparable elements. *)

let insert (<=) x ys =
  (* If [x] is subsumed by some element [y] of [ys], then there is nothing
     to do. In particular, no element [y] of [ys] can be subsumed by [x],
     since the elements of [ys] are pairwise incomparable. *)
  if List.exists (fun y -> x <= y) ys then
    ys
  (* Or [x] must be inserted, and any element [y] of [ys] that is subsumed
     by [x] must be removed. *)
  else
    x :: List.filter (fun y -> not (y <= x)) ys

(* Suppose [xs] is an arbitrary list of elements. Then [trim (<=) xs] is the
   sublist of the elements of [xs] that are maximal with respect to the
   partial order [<=]. In other words, it is a sublist where every element
   that is less than some other element has been removed. *)

(* One might wish to define [trim] using [List.filter] to keep just the
   maximal elements, but it is not so easy to say "keep an element only
   if it is not subsumed by some *other* element of the list". Instead,
   we iterate [insert]. *)

let trim (<=) xs =
  List.fold_right (insert (<=)) xs []

let rec dup1 cmp x ys =
  match ys with
  | [] ->
      None
  | y :: ys ->
      if cmp x y = 0 then
        Some x
      else
        dup1 cmp y ys

let dup cmp xs =
  match xs with
  | [] ->
      None
  | x :: xs ->
      dup1 cmp x xs

let once x y =
  let s = ref x in
  fun () ->
    let result = !s in
    s := y;
    result

module ListExtras = struct
  let rec equal (=) xs ys =
    match xs, ys with
    | [], [] ->
        true
    | x :: xs, y :: ys ->
        x = y && equal (=) xs ys
    | _ :: _, []
    | [], _ :: _ ->
        false
  let hash hash xs =
    Hashtbl.hash (List.map hash xs)
end

let nth = function
  | 1 -> "first"
  | 2 -> "second"
  | 3 -> "third"
  | i -> Printf.sprintf "%dth" i

let count = function
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | i -> Printf.sprintf "%d" i

(* [digits n] computes how many decimal digits are involved in the
   decimal representation of the integer [n]. *)

let digits n =
  let rec loop accu n =
    if n < 10 then
      accu + 1
    else
      loop (accu + 1) (n / 10)
  in
  loop 0 n

(* [pad n s] pads the string [s] with zeroes in front so that its length
   is [n]. *)

let pad n s =
  String.make (n - String.length s) '0' ^ s

let padded_index n i =
  pad (digits n) (Printf.sprintf "%d" i)
