open! Import


(* [t] stores the [t.length] queue elements at consecutive increasing indices of [t.elts],
   mod the capacity of [t], which is [Option_array.length t.elts].  The capacity is
   required to be a power of two (user-requested capacities are rounded up to the nearest
   power), so that mod can quickly be computed using [land t.mask], where [t.mask =
   capacity t - 1].  So, queue element [i] is at [t.elts.( (t.front + i) land t.mask )].

   [num_mutations] is used to detect modification during iteration. *)
type 'a t =
  { mutable num_mutations : int
  ; mutable front : int
  ; mutable mask : int
  ; mutable length : int
  ; mutable elts : 'a Option_array.t
  }
[@@deriving_inline sexp_of]

let sexp_of_t : 'a. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t =
  fun _of_a -> function
    | { num_mutations = v_num_mutations
      ; front = v_front
      ; mask = v_mask
      ; length = v_length
      ; elts = v_elts
      } ->
      let bnds = [] in
      let bnds =
        let arg = Option_array.sexp_of_t _of_a v_elts in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "elts"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_int v_length in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "length"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_int v_mask in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "mask"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_int v_front in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "front"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_int v_num_mutations in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "num_mutations"; arg ]
        :: bnds
      in
      Ppx_sexp_conv_lib.Sexp.List bnds
;;

[@@@end]

module type S = Queue_intf.S

let inc_num_mutations t = t.num_mutations <- t.num_mutations + 1
let capacity t = t.mask + 1
let elts_index t i = (t.front + i) land t.mask
let unsafe_get t i = Option_array.unsafe_get_some_exn t.elts (elts_index t i)
let unsafe_is_set t i = Option_array.unsafe_is_some t.elts (elts_index t i)
let unsafe_set t i a = Option_array.unsafe_set_some t.elts (elts_index t i) a
let unsafe_unset t i = Option_array.unsafe_set_none t.elts (elts_index t i)

let check_index_exn t i =
  if i < 0 || i >= t.length
  then
    Error.raise_s
      (Sexp.message
         "Queue index out of bounds"
         [ "index", i |> Int.sexp_of_t; "length", t.length |> Int.sexp_of_t ])
;;

let get t i =
  check_index_exn t i;
  unsafe_get t i
;;

let set t i a =
  check_index_exn t i;
  inc_num_mutations t;
  unsafe_set t i a
;;

let is_empty t = t.length = 0
let length { length; _ } = length

let ensure_no_mutation t num_mutations =
  if t.num_mutations <> num_mutations
  then
    Error.raise_s
      (Sexp.message
         "mutation of queue during iteration"
         [ "", t |> sexp_of_t (fun _ -> Sexp.Atom "_") ])
;;

let compare =
  let rec unsafe_compare_from compare_elt pos ~t1 ~t2 ~len1 ~len2 ~mut1 ~mut2 =
    match pos = len1, pos = len2 with
    | true, true -> 0
    | true, false -> -1
    | false, true -> 1
    | false, false ->
      let x = compare_elt (unsafe_get t1 pos) (unsafe_get t2 pos) in
      ensure_no_mutation t1 mut1;
      ensure_no_mutation t2 mut2;
      (match x with
       | 0 -> unsafe_compare_from compare_elt (pos + 1) ~t1 ~t2 ~len1 ~len2 ~mut1 ~mut2
       | n -> n)
  in
  fun compare_elt t1 t2 ->
    if phys_equal t1 t2
    then 0
    else
      unsafe_compare_from
        compare_elt
        0
        ~t1
        ~t2
        ~len1:t1.length
        ~len2:t2.length
        ~mut1:t1.num_mutations
        ~mut2:t2.num_mutations
;;

let equal =
  let rec unsafe_equal_from equal_elt pos ~t1 ~t2 ~mut1 ~mut2 ~len =
    pos = len
    ||
    let b = equal_elt (unsafe_get t1 pos) (unsafe_get t2 pos) in
    ensure_no_mutation t1 mut1;
    ensure_no_mutation t2 mut2;
    b && unsafe_equal_from equal_elt (pos + 1) ~t1 ~t2 ~mut1 ~mut2 ~len
  in
  fun equal_elt t1 t2 ->
    phys_equal t1 t2
    ||
    let len1 = t1.length in
    let len2 = t2.length in
    len1 = len2
    && unsafe_equal_from
         equal_elt
         0
         ~t1
         ~t2
         ~len:len1
         ~mut1:t1.num_mutations
         ~mut2:t2.num_mutations
;;

let invariant invariant_a t =
  let { num_mutations; mask = _; elts; front; length } = t in
  assert (front >= 0);
  assert (front < capacity t);
  let capacity = capacity t in
  assert (capacity = Option_array.length elts);
  assert (capacity >= 1);
  assert (Int.is_pow2 capacity);
  assert (length >= 0);
  assert (length <= capacity);
  for i = 0 to capacity - 1 do
    if i < t.length
    then (
      invariant_a (unsafe_get t i);
      ensure_no_mutation t num_mutations)
    else assert (not (unsafe_is_set t i))
  done
;;

let create (type a) ?capacity () : a t =
  let capacity =
    match capacity with
    | None -> 1
    | Some capacity ->
      if capacity < 0
      then
        Error.raise_s
          (Sexp.message
             "cannot have queue with negative capacity"
             [ "capacity", capacity |> Int.sexp_of_t ])
      else if capacity = 0
      then 1
      else Int.ceil_pow2 capacity
  in
  { num_mutations = 0
  ; front = 0
  ; mask = capacity - 1
  ; length = 0
  ; elts = Option_array.create ~len:capacity
  }
;;

let blit_to_array ~src dst =
  assert (src.length <= Option_array.length dst);
  let front_len = Int.min src.length (capacity src - src.front) in
  let rest_len = src.length - front_len in
  Option_array.blit ~len:front_len ~src:src.elts ~src_pos:src.front ~dst ~dst_pos:0;
  Option_array.blit ~len:rest_len ~src:src.elts ~src_pos:0 ~dst ~dst_pos:front_len
;;

let set_capacity t desired_capacity =
  (* We allow arguments less than 1 to [set_capacity], but translate them to 1 to simplify
     the code that relies on the array length being a power of 2. *)
  inc_num_mutations t;
  let new_capacity = Int.ceil_pow2 (max 1 (max desired_capacity t.length)) in
  if new_capacity <> capacity t
  then (
    let dst = Option_array.create ~len:new_capacity in
    blit_to_array ~src:t dst;
    t.front <- 0;
    t.mask <- new_capacity - 1;
    t.elts <- dst)
;;

let enqueue t a =
  inc_num_mutations t;
  if t.length = capacity t then set_capacity t (2 * t.length);
  unsafe_set t t.length a;
  t.length <- t.length + 1
;;

let dequeue_nonempty t =
  inc_num_mutations t;
  let elts = t.elts in
  let front = t.front in
  let res = Option_array.get_some_exn elts front in
  Option_array.set_none elts front;
  t.front <- elts_index t 1;
  t.length <- t.length - 1;
  res
;;

let dequeue_exn t = if is_empty t then raise Caml.Queue.Empty else dequeue_nonempty t
let dequeue t = if is_empty t then None else Some (dequeue_nonempty t)
let front_nonempty t = Option_array.unsafe_get_some_exn t.elts t.front
let last_nonempty t = unsafe_get t (t.length - 1)
let peek t = if is_empty t then None else Some (front_nonempty t)
let peek_exn t = if is_empty t then raise Caml.Queue.Empty else front_nonempty t
let last t = if is_empty t then None else Some (last_nonempty t)
let last_exn t = if is_empty t then raise Caml.Queue.Empty else last_nonempty t

let clear t =
  inc_num_mutations t;
  if t.length > 0
  then (
    for i = 0 to t.length - 1 do
      unsafe_unset t i
    done;
    t.length <- 0;
    t.front <- 0)
;;

let blit_transfer ~src ~dst ?len () =
  inc_num_mutations src;
  inc_num_mutations dst;
  let len =
    match len with
    | None -> src.length
    | Some len ->
      if len < 0
      then
        Error.raise_s
          (Sexp.message
             "Queue.blit_transfer: negative length"
             [ "length", len |> Int.sexp_of_t ]);
      min len src.length
  in
  if len > 0
  then (
    set_capacity dst (max (capacity dst) (dst.length + len));
    let dst_start = dst.front + dst.length in
    for i = 0 to len - 1 do
      (* This is significantly faster than simply [enqueue dst (dequeue_nonempty src)] *)
      let src_i = (src.front + i) land src.mask in
      let dst_i = (dst_start + i) land dst.mask in
      Option_array.unsafe_set_some
        dst.elts
        dst_i
        (Option_array.unsafe_get_some_exn src.elts src_i);
      Option_array.unsafe_set_none src.elts src_i
    done;
    dst.length <- dst.length + len;
    src.front <- (src.front + len) land src.mask;
    src.length <- src.length - len)
;;

let enqueue_all t l =
  (* Traversing the list up front to compute its length is probably (but not definitely)
     better than doubling the underlying array size several times for large queues. *)
  set_capacity t (Int.max (capacity t) (t.length + List.length l));
  List.iter l ~f:(fun x -> enqueue t x)
;;

let fold t ~init ~f =
  if t.length = 0
  then init
  else (
    let num_mutations = t.num_mutations in
    let r = ref init in
    for i = 0 to t.length - 1 do
      r := f !r (unsafe_get t i);
      ensure_no_mutation t num_mutations
    done;
    !r)
;;

let foldi t ~init ~f =
  let i = ref 0 in
  fold t ~init ~f:(fun acc a ->
    let acc = f !i acc a in
    i := !i + 1;
    acc)
;;


(* [iter] is implemented directly because implementing it in terms of [fold] is
   slower. *)
let iter t ~f =
  let num_mutations = t.num_mutations in
  for i = 0 to t.length - 1 do
    f (unsafe_get t i);
    ensure_no_mutation t num_mutations
  done
;;

let iteri t ~f =
  let num_mutations = t.num_mutations in
  for i = 0 to t.length - 1 do
    f i (unsafe_get t i);
    ensure_no_mutation t num_mutations
  done
;;

module C = Indexed_container.Make (struct
    type nonrec 'a t = 'a t

    let fold = fold
    let iter = `Custom iter
    let length = `Custom length
    let foldi = `Custom foldi
    let iteri = `Custom iteri
  end)

let count = C.count
let exists = C.exists
let find = C.find
let find_map = C.find_map
let fold_result = C.fold_result
let fold_until = C.fold_until
let for_all = C.for_all
let max_elt = C.max_elt
let mem = C.mem
let min_elt = C.min_elt
let sum = C.sum
let to_list = C.to_list
let counti = C.counti
let existsi = C.existsi
let find_mapi = C.find_mapi
let findi = C.findi
let for_alli = C.for_alli


(* For [concat_map], [filter_map], and [filter], we don't create [t_result] with [t]'s
   capacity because we have no idea how many elements [t_result] will ultimately hold. *)
let concat_map t ~f =
  let t_result = create () in
  iter t ~f:(fun a -> List.iter (f a) ~f:(fun b -> enqueue t_result b));
  t_result
;;

let concat_mapi t ~f =
  let t_result = create () in
  iteri t ~f:(fun i a -> List.iter (f i a) ~f:(fun b -> enqueue t_result b));
  t_result
;;

let filter_map t ~f =
  let t_result = create () in
  iter t ~f:(fun a ->
    match f a with
    | None -> ()
    | Some b -> enqueue t_result b);
  t_result
;;

let filter_mapi t ~f =
  let t_result = create () in
  iteri t ~f:(fun i a ->
    match f i a with
    | None -> ()
    | Some b -> enqueue t_result b);
  t_result
;;

let filter t ~f =
  let t_result = create () in
  iter t ~f:(fun a -> if f a then enqueue t_result a);
  t_result
;;

let filteri t ~f =
  let t_result = create () in
  iteri t ~f:(fun i a -> if f i a then enqueue t_result a);
  t_result
;;

let filter_inplace t ~f =
  let t2 = filter t ~f in
  clear t;
  blit_transfer ~src:t2 ~dst:t ()
;;

let filteri_inplace t ~f =
  let t2 = filteri t ~f in
  clear t;
  blit_transfer ~src:t2 ~dst:t ()
;;

let copy src =
  let dst = create ~capacity:src.length () in
  blit_to_array ~src dst.elts;
  dst.length <- src.length;
  dst
;;

let of_list l =
  (* Traversing the list up front to compute its length is probably (but not definitely)
     better than doubling the underlying array size several times for large queues. *)
  let t = create ~capacity:(List.length l) () in
  List.iter l ~f:(fun x -> enqueue t x);
  t
;;

(* The queue [t] returned by [create] will have [t.length = 0], [t.front = 0], and
   [capacity t = Int.ceil_pow2 len].  So, we only have to set [t.length] to [len] after
   the blit to maintain all the invariants: [t.length] is equal to the number of elements
   in the queue, [t.front] is the array index of the first element in the queue, and
   [capacity t = Option_array.length t.elts]. *)
let init len ~f =
  if len < 0
  then
    Error.raise_s
      (Sexp.message "Queue.init: negative length" [ "length", len |> Int.sexp_of_t ]);
  let t = create ~capacity:len () in
  assert (Option_array.length t.elts >= len);
  for i = 0 to len - 1 do
    Option_array.unsafe_set_some t.elts i (f i)
  done;
  t.length <- len;
  t
;;

let of_array a = init (Array.length a) ~f:(Array.unsafe_get a)
let to_array t = Array.init t.length ~f:(fun i -> unsafe_get t i)

let map ta ~f =
  let num_mutations = ta.num_mutations in
  let tb = create ~capacity:ta.length () in
  tb.length <- ta.length;
  for i = 0 to ta.length - 1 do
    let b = f (unsafe_get ta i) in
    ensure_no_mutation ta num_mutations;
    Option_array.unsafe_set_some tb.elts i b
  done;
  tb
;;

let mapi t ~f =
  let i = ref 0 in
  map t ~f:(fun a ->
    let result = f !i a in
    i := !i + 1;
    result)
;;

let singleton x =
  let t = create () in
  enqueue t x;
  t
;;

let sexp_of_t sexp_of_a t = to_list t |> List.sexp_of_t sexp_of_a
let t_of_sexp a_of_sexp sexp = List.t_of_sexp a_of_sexp sexp |> of_list
