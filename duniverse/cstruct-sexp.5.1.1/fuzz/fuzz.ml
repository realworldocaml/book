open Crowbar

[@@@warning "-3"]

let create x =
  match Cstruct.create x with
  | c -> assert (x >= 0); c
  | exception Invalid_argument _ -> assert (x <= 0); bad_test ()

let create_sub x start len =
  try
    let c = Cstruct.create_unsafe x in
    for i = 0 to len - 1 do
      Cstruct.set_uint8 c i i
    done;
    Cstruct.sub c start len
  with Invalid_argument _ -> bad_test ()

let cstruct = choose [
    map [int8] create;
    map [range 0x10000; int; int] create_sub;
  ]

let bytes = map [bytes] Bytes.unsafe_of_string

let buffer = map [uint8] Bigarray.(Array1.create Char c_layout)

let pp_cstruct f c = Format.pp_print_string f (Cstruct.debug c)

let check_within ~base x =
  check Cstruct.(base.off <= x.off);
  check Cstruct.(base.off + base.len >= x.off + x.len);
  check Cstruct.(x.len >= 0 && x.len <= base.len)

let () =
  assert (Array.length Sys.argv = 2);   (* Prevent accidentally running in quickcheck mode *)
  add_test ~name:"blit" [cstruct; int; cstruct; int; int] (fun src srcoff dst dstoff len ->
      try Cstruct.blit src srcoff dst dstoff len
      with Invalid_argument _ ->
        check (srcoff < 0 || srcoff > Cstruct.len src ||
               dstoff < 0 || dstoff > Cstruct.len src ||
               len < 0 ||
               len > Cstruct.len src - srcoff ||
               len > Cstruct.len dst - dstoff)
    );
  add_test ~name:"sexp" [buffer] (fun b ->
      b |> Cstruct_sexp.sexp_of_buffer |> Cstruct_sexp.buffer_of_sexp
      |> check_eq
        ~cmp:(fun x y -> Cstruct.compare (Cstruct.of_bigarray x) (Cstruct.of_bigarray y))
        b
    );
  add_test ~name:"of_bigarray" [buffer; option int; option int] (fun b off len ->
      match Cstruct.of_bigarray b ?off ?len with
      | c -> check (Cstruct.len c <= Bigarray.Array1.dim b)
      | exception Invalid_argument _ -> ()
    );
  add_test ~name:"get_char" [cstruct; int] (fun c off ->
      let in_range = off >= 0 && off < Cstruct.len c in
      match Cstruct.get_char c off with
      | _ -> check in_range
      | exception Invalid_argument _ -> check (not in_range)
    );
  add_test ~name:"set_char" [cstruct; int] (fun c off ->
      let in_range = off >= 0 && off < Cstruct.len c in
      match Cstruct.set_char c off 'x' with
      | () -> check in_range
      | exception Invalid_argument _ -> check (not in_range)
    );
  add_test ~name:"sub" [cstruct; int; int] (fun base off len ->
      match Cstruct.sub base off len with
      | sub ->
        check_within ~base sub;
        check (Cstruct.len sub = len)
      | exception Invalid_argument _ ->
        check (off < 0 || len < 0 || off + len < 0 || off + len > Cstruct.len base)
    );
  add_test ~name:"shift" [cstruct; int] (fun base off ->
      match Cstruct.shift base off with
      | sub ->
        check_within ~base sub;
        check (Cstruct.len sub = max (Cstruct.len base - off) 0);
      | exception Invalid_argument _ -> check (off < 0 || off > Cstruct.len base)
    );
  add_test ~name:"copy" [cstruct; int; int] (fun base off len ->
      match Cstruct.copy base off len with
      | x ->
        check (String.length x = len);
        check (String.equal x (Cstruct.sub base off len |> Cstruct.to_string))
      | exception Invalid_argument _ ->
        check (off < 0 || len < 0 || off + len < 0 || off + len > Cstruct.len base)
    );
  add_test ~name:"blit_from_bytes" [bytes; int; cstruct; int; int] (fun src srcoff dst dstoff len ->
      match Cstruct.blit_from_bytes src srcoff dst dstoff len with
      | () -> check (Cstruct.equal (Cstruct.sub (Cstruct.of_bytes src) srcoff len)
                                   (Cstruct.sub dst dstoff len))
      | exception Invalid_argument _ ->
        check (srcoff < 0 || srcoff > Bytes.length src ||
               dstoff < 0 || dstoff > Bytes.length src ||
               len < 0 ||
               len > Bytes.length src - srcoff ||
               len > Cstruct.len dst - dstoff)
    );
  add_test ~name:"blit_to_bytes" [cstruct; int; bytes; int; int] (fun src srcoff dst dstoff len ->
      match Cstruct.blit_to_bytes src srcoff dst dstoff len with
      | () -> check (Cstruct.equal (Cstruct.sub src srcoff len)
                                   (Cstruct.sub (Cstruct.of_bytes dst) dstoff len))
      | exception Invalid_argument _ ->
        check (srcoff < 0 || srcoff > Cstruct.len src ||
               dstoff < 0 || dstoff > Cstruct.len src ||
               len < 0 ||
               len > Cstruct.len src - srcoff ||
               len > Bytes.length dst - dstoff)
    );
  add_test ~name:"memset" [cstruct; int; int] (fun c x i ->
      guard (i >= 0 && i < Cstruct.len c);
      Cstruct.memset c x;
      check (Cstruct.get_uint8 c i = x land 0xff)
    );
  add_test ~name:"set_len" [cstruct; int] (fun base len ->
      match[@ocaml.warning "-3"] Cstruct.set_len base len with
      | x ->
        (* check_within ~base x; *)   (* Maybe deprecate set_len extending the allocation? *)
        check (Cstruct.len x >= 0);
        check (Cstruct.len x <= Bigarray.Array1.dim (base.Cstruct.buffer));
        check (Cstruct.len x = len)
      | exception Invalid_argument _ -> ()
    );
  add_test ~name:"add_len" [cstruct; int] (fun base len ->
      match[@ocaml.warning "-3"] Cstruct.add_len base len with
      | x ->
        check (Cstruct.len x >= 0);
        check (Cstruct.len x <= Bigarray.Array1.dim (base.Cstruct.buffer));
        check (Cstruct.len x = Cstruct.len base + len)
      | exception Invalid_argument _ -> ()
    );
  add_test ~name:"split" [cstruct; option int; int] (fun base start len ->
      match Cstruct.split ?start base len  with
      | c1, c2 ->
        check_within ~base c1;
        check_within ~base c2;
        let start = match start with None -> 0 | Some x -> x in
        check (Cstruct.equal (Cstruct.sub base start len) c1);
        check (Cstruct.equal (Cstruct.shift base (start + len)) c2)
      | exception Invalid_argument _ -> ()
    );
  add_test ~name:"BE.set_uint64" [cstruct; int] (fun c off ->
      let in_range = off >= 0 && off < Cstruct.len c - 7 in
      match Cstruct.BE.set_uint64 c off 42L with
      | () -> check in_range
      | exception Invalid_argument _ -> check (not in_range)
    );
  add_test ~name:"lenv" [list cstruct] (fun cs ->
      check (Cstruct.lenv cs >= 0)
    );
  add_test ~name:"copyv" [list cstruct] (fun cs ->
      check (String.equal (Cstruct.copyv cs) (Cstruct.concat cs |> Cstruct.to_string))
    );
  add_test ~name:"fillv" [list cstruct; cstruct] (fun src dst ->
      let copied, rest = Cstruct.fillv ~src ~dst in
      check (copied + Cstruct.lenv rest = Cstruct.lenv src);
      (* OCaml tends to underestimate how much space bigarrays are using: *)
      Gc.minor ()
    );
  add_test ~name:"concat" [list cstruct] (fun cs ->
      let x = Cstruct.concat cs in
      check (Cstruct.len x = Cstruct.lenv cs)
    );
