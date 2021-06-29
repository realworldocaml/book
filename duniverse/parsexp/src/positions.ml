(* This module builds a buffer of "instructions", in order to represent a compact sequence
   of delimiting positions and newlines. The parser stores the positions of each:

   - newline
   - beginning of atom
   - end of atom
   - left parenthesis
   - right parenthesis

   Instructions are encoded as a sequence bits. The next instruction is determined by
   looking at the next few bits:

   - bit 0 represents a saved position followed by an offset increment
   - bits 10 represent an offset increment
   - bits 110 are followed by 5 bits of payload. The 5-bit payloads of any subsequent 110-
     instructions are squashed to form a number (least significant 5-bit chunk first).
     This number + 5 represents an offset increment
   - bits 1110 marks the beginning of a new line (with offset incremented)
   - bits 1111 represent a position saved twice followed by an offset increment

   For instance let's consider the following sexp:

   {[
     {|
(abc
      "foo
 bar"
)
|}
   ]}

   the sequence of instructions to record in order to reconstruct the position of any
   sub-sexp is:

   - 0         save position and advance 1: first '('
   - 0         save position and advance 1: start of "abc"
   - 10        advance 1
   - 0         save position and advance 1: end of "abc"
   - 1110      newline
   - 1100_0001 advance 6
   - 0         save position and advance 1: start of "foo\n  bar"
   - 10        advance 1
   - 10        advance 1
   - 10        advance 1
   - 1110      newline
   - 1100_0000 advance 5
   - 0         save position and advance 1: end of "foo\n  bar"
   - 1110      newline
   - 0         save position and advance 1: last ')'

   (we save the position after the closing parenthesis)

   The total sequence is 42 bits, so we need 6 bytes to store it

   The sequence of bits is encoded as a sequence of 16-bit values, where the earlier bits
   are most significant.

   Note that the parser stores the end positions as inclusive. This way only single
   character atoms require a double positions. If we were storing end positions as
   exclusive, we would need double positions for [)(] and [a(], which are likely to be
   frequent in s-expressions printed with the non [_hum] printer. We expect single
   character atoms to be less frequent so it makes sense to penalize them instead.
*)

open! Import

type pos =
  { line : int
  ; col : int
  ; offset : int
  }
[@@deriving_inline sexp_of]

let sexp_of_pos =
  (function
    | { line = v_line; col = v_col; offset = v_offset } ->
      let bnds = [] in
      let bnds =
        let arg = sexp_of_int v_offset in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "offset"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_int v_col in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "col"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_int v_line in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "line"; arg ] :: bnds
      in
      Ppx_sexp_conv_lib.Sexp.List bnds
      : pos -> Ppx_sexp_conv_lib.Sexp.t)
;;

[@@@end]

let compare_pos = Caml.compare
let beginning_of_file = { line = 1; col = 0; offset = 0 }
let shift_pos pos ~cols = { pos with col = pos.col + cols; offset = pos.offset + cols }

type range =
  { start_pos : pos
  ; end_pos : pos
  }
[@@deriving_inline sexp_of]

let sexp_of_range =
  (function
    | { start_pos = v_start_pos; end_pos = v_end_pos } ->
      let bnds = [] in
      let bnds =
        let arg = sexp_of_pos v_end_pos in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "end_pos"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_pos v_start_pos in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "start_pos"; arg ]
        :: bnds
      in
      Ppx_sexp_conv_lib.Sexp.List bnds
      : range -> Ppx_sexp_conv_lib.Sexp.t)
;;

[@@@end]

let compare_range = Caml.compare

let make_range_incl ~start_pos ~last_pos =
  { start_pos; end_pos = shift_pos last_pos ~cols:1 }
;;

module Chunk : sig
  (** Represents an array of [length/2] signed 16-bit values *)
  type t

  (** Length in bytes. *)
  val length : int

  val alloc : unit -> t

  (** [get16 ~pos] and [set16 ~pos] manipulate the [pos/2]th stored value.
      [pos] must be even.
      [set16 x] only uses the 16 least significant bits of [x]. *)
  val get16 : t -> pos:int -> int

  val set16 : t -> pos:int -> int -> unit
end = struct
  type t = bytes

  (* OCaml strings always waste two bytes at the end, so we take a power of two minus two
     to be sure we don't waste space. *)
  let length = 62
  let alloc () = Bytes.create length
  let get16 = Bytes0.get16
  let set16 = Bytes0.set16

  (* If we want to make a [Positions.t] serializable:

     {[
       external bswap16 : int -> int = "%bswap16";;

       let get16 =
         if Caml.Sys.arch_big_endian then
           fun buf ~pos -> get16 buf ~pos |> bswap16
         else
           get16

       let set16 =
         if Caml.Sys.arch_big_endian then
           fun buf ~pos x -> set16 buf ~pos (bswap16 x)
         else
           set16
     ]}

  *)
end

type t_ =
  { chunks : Chunk.t list
  ; (* [num_bytes * 8 + extra_bits] is the number of bits stored in [chunks].
       The last [extra_bits] bits will be stored as the *least* significant bits
       of the appropriate pair of bytes of the last chunk. *)
    num_bytes : int
  ; extra_bits : int
  ; initial_pos : pos
  }

type t = t_ Lazy.t

let memory_footprint_in_bytes (lazy t) =
  let num_fields = 4 in
  let header_words = 1 in
  let word_bytes =
    match Sys.word_size with
    | 32 -> 4
    | 64 -> 8
    | _ -> assert false
  in
  let chunk_words =
    let div_ceil a b = (a + b - 1) / b in
    let n =
      div_ceil
        (Chunk.length
         + 1 (* NUL terminating bytes *)
         + 1 (* number of wasted bytes to fill a word *))
        word_bytes
    in
    n + header_words
  in
  let pos_fields = 3 in
  let pos_words = header_words + pos_fields in
  let list_cons_words = header_words + 2 in
  (header_words
   + num_fields
   + pos_words
   + (List.length t.chunks * (chunk_words + list_cons_words)))
  * word_bytes
;;

module Builder = struct
  type t =
    { mutable chunk : Chunk.t
    ; mutable chunk_pos : int
    ; mutable filled_chunks : Chunk.t list (* Filled chunks in reverse order *)
    ; mutable offset : int
    (* Offset of the last saved position or newline plus
       one, or [initial_pos] *)
    ; mutable int_buf : int
    (* the [num_bits] least significant bits of [int_buf]
       are the bits not yet pushed to [chunk]. *)
    ; mutable num_bits : int (* number of bits stored in [int_buf] *)
    ; mutable initial_pos : pos
    }

  let invariant t =
    assert (t.chunk_pos >= 0 && t.chunk_pos <= Chunk.length);
    assert (t.offset >= t.initial_pos.offset);
    assert (t.num_bits <= 15)
  ;;

  let check_invariant = false
  let invariant t = if check_invariant then invariant t

  let create ?(initial_pos = beginning_of_file) () =
    { chunk = Chunk.alloc ()
    ; chunk_pos = 0
    ; filled_chunks = []
    ; offset = initial_pos.offset
    ; int_buf = 0
    ; num_bits = 0
    ; initial_pos
    }
  ;;

  let reset t (pos : pos) =
    (* We need a new chunk as [contents] keeps the current chunk in the closure of the
       lazy value. *)
    t.chunk <- Chunk.alloc ();
    t.chunk_pos <- 0;
    t.filled_chunks <- [];
    t.offset <- pos.offset;
    t.int_buf <- 0;
    t.num_bits <- 0;
    t.initial_pos <- pos
  ;;

  let[@inlined never] alloc_new_chunk t =
    t.filled_chunks <- t.chunk :: t.filled_chunks;
    t.chunk <- Chunk.alloc ();
    t.chunk_pos <- 0
  ;;

  let add_uint16 t n =
    if t.chunk_pos = Chunk.length then alloc_new_chunk t;
    Chunk.set16 t.chunk ~pos:t.chunk_pos n
  ;;

  let add_bits t n ~num_bits =
    let int_buf = (t.int_buf lsl num_bits) lor n in
    let num_bits = t.num_bits + num_bits in
    t.int_buf <- int_buf;
    if num_bits < 16
    then t.num_bits <- num_bits
    else (
      let num_bits = num_bits - 16 in
      t.num_bits <- num_bits;
      add_uint16 t (int_buf lsr num_bits);
      t.chunk_pos <- t.chunk_pos + 2
      (* no need to clear the bits of int_buf we just wrote, as further set16 will ignore
         these extra bits. *))
  ;;

  let contents t =
    (* Flush the current [t.int_buf] *)
    add_uint16 t t.int_buf;
    let rev_chunks = t.chunk :: t.filled_chunks in
    let chunk_pos = t.chunk_pos in
    let extra_bits = t.num_bits in
    let initial_pos = t.initial_pos in
    lazy
      { chunks = List.rev rev_chunks
      ; num_bytes = ((List.length rev_chunks - 1) * Chunk.length) + chunk_pos
      ; extra_bits
      ; initial_pos
      }
  ;;

  let long_shift t n =
    let n = ref (n - 5) in
    while !n > 0 do
      add_bits t (0b1100_0000 lor (!n land 0b0001_1111)) ~num_bits:8;
      n := !n lsr 5
    done
  ;;

  (* precondition: n >= 5 *)
  let[@inlined never] add_gen_slow t n ~instr ~instr_bits =
    long_shift t n;
    add_bits t instr ~num_bits:instr_bits
  ;;

  let shift4 = 0b10_10_10_10

  let[@inline always] add_gen t ~offset ~instr ~instr_bits =
    invariant t;
    let n = offset - t.offset in
    t.offset <- offset + 1;
    match n with
    | 0 | 1 | 2 | 3 | 4 ->
      let num_bits = (n lsl 1) + instr_bits in
      add_bits
        t
        ((shift4 lsl instr_bits) lor instr land ((1 lsl num_bits) - 1))
        ~num_bits
    | 5
    | 6
    | 7
    | 8
    | 9
    | 10
    | 11
    | 12
    | 13
    | 14
    | 15
    | 16
    | 17
    | 18
    | 19
    | 20
    | 21
    | 22
    | 23
    | 24
    | 25
    | 26
    | 27
    | 28
    | 29
    | 30
    | 31
    | 32
    | 33
    | 34
    | 35
    | 36 ->
      add_bits
        t
        (((0b1100_0000 lor (n - 5)) lsl instr_bits) lor instr)
        ~num_bits:(8 + instr_bits)
    | _ ->
      if n < 0 then invalid_arg "Parsexp.Positions.add_gen";
      add_gen_slow t n ~instr ~instr_bits
  ;;

  let add t ~offset = add_gen t ~offset ~instr:0b0 ~instr_bits:1
  let add_twice t ~offset = add_gen t ~offset ~instr:0b1111 ~instr_bits:4
  let add_newline t ~offset = add_gen t ~offset ~instr:0b1110 ~instr_bits:4
end

type positions = t

module Iterator : sig
  type t

  val create : positions -> t

  exception No_more

  (* [advance t ~skip] ignores [skip] saved positions and returns the next saved position.
     Raises [No_more] when reaching the end of the position set. *)
  val advance_exn : t -> skip:int -> pos
end = struct
  type t =
    { mutable chunk : Chunk.t
    ; mutable chunks : Chunk.t list
    ; (* [num_bytes * 8 + extra_bits] is the number of bits available from [instr_pos] in
         [chunk :: chunks]. *)
      mutable num_bytes : int
    ; extra_bits : int
    ; mutable instr_pos : int (* position in [chunk] *)
    ; mutable offset : int
    ; mutable line : int
    ; mutable bol : int
    ; mutable int_buf : int
    ; mutable num_bits : int (* Number of bits not yet consumed in [int_buf] *)
    ; mutable pending : pos option
    }

  let create ((lazy p) : positions) =
    match p.chunks with
    | [] -> assert false
    | chunk :: chunks ->
      { chunk
      ; chunks
      ; num_bytes = p.num_bytes
      ; extra_bits = p.extra_bits
      ; instr_pos = 0
      ; offset = p.initial_pos.offset
      ; line = p.initial_pos.line
      ; bol = p.initial_pos.offset - p.initial_pos.col
      ; int_buf = 0
      ; num_bits = 0
      ; pending = None
      }
  ;;

  exception No_more

  let no_more () = raise_notrace No_more

  let[@inlined never] fetch_chunk t =
    match t.chunks with
    | [] -> assert false
    | chunk :: chunks ->
      t.instr_pos <- 0;
      t.num_bytes <- t.num_bytes - Chunk.length;
      t.chunk <- chunk;
      t.chunks <- chunks
  ;;

  let fetch t =
    if t.instr_pos > t.num_bytes then no_more ();
    if t.instr_pos = Chunk.length then fetch_chunk t;
    let v = Chunk.get16 t.chunk ~pos:t.instr_pos in
    let added_bits = if t.instr_pos = t.num_bytes then t.extra_bits else 16 in
    t.int_buf <- (t.int_buf lsl added_bits) lor (v land ((1 lsl added_bits) - 1));
    t.num_bits <- t.num_bits + added_bits;
    t.instr_pos <- t.instr_pos + 2
  ;;

  let next_instruction_bits t ~num_bits =
    if t.num_bits < num_bits
    then (
      fetch t;
      if t.num_bits < num_bits then no_more ());
    let n = (t.int_buf lsr (t.num_bits - num_bits)) land ((1 lsl num_bits) - 1) in
    t.num_bits <- t.num_bits - num_bits;
    n
  ;;

  (* [offset_shift] and [offset_shift_num_bits] encode the offset number
     specified by the immediately preceding [110] instructions. *)
  let rec advance t ~skip ~offset_shift ~offset_shift_num_bits =
    match next_instruction_bits t ~num_bits:1 with
    | 0 ->
      (* bit seq 0 -> new item *)
      let offset = t.offset + offset_shift in
      t.offset <- offset + 1;
      if skip = 0
      then { line = t.line; col = offset - t.bol; offset }
      else advance t ~skip:(skip - 1) ~offset_shift:0 ~offset_shift_num_bits:0
    | _ ->
      (match next_instruction_bits t ~num_bits:1 with
       | 0 ->
         (* bit seq 10 -> shift *)
         t.offset <- t.offset + offset_shift + 1;
         advance t ~skip ~offset_shift:0 ~offset_shift_num_bits:0
       | _ ->
         (match next_instruction_bits t ~num_bits:1 with
          | 0 ->
            (* bit seq 110 -> long shift *)
            let n = next_instruction_bits t ~num_bits:5 in
            let offset_shift = if offset_shift_num_bits = 0 then 5 else offset_shift in
            advance
              t
              ~skip
              ~offset_shift:(offset_shift + (n lsl offset_shift_num_bits))
              ~offset_shift_num_bits:(offset_shift_num_bits + 5)
          | _ ->
            (match next_instruction_bits t ~num_bits:1 with
             | 0 ->
               (* bit seq 1110 -> newline *)
               t.offset <- t.offset + offset_shift + 1;
               t.bol <- t.offset;
               t.line <- t.line + 1;
               advance t ~skip ~offset_shift:0 ~offset_shift_num_bits:0
             | _ ->
               (* bit seq 1111 -> 2 new items *)
               let offset = t.offset + offset_shift in
               t.offset <- offset + 1;
               if skip <= 1
               then (
                 let pos = { line = t.line; col = offset - t.bol; offset } in
                 if skip = 0 then t.pending <- Some pos;
                 pos)
               else advance t ~skip:(skip - 2) ~offset_shift:0 ~offset_shift_num_bits:0)))
  ;;

  let advance_exn t ~skip =
    match t.pending with
    | Some pos ->
      t.pending <- None;
      if skip = 0
      then pos
      else advance t ~skip:(skip - 1) ~offset_shift:0 ~offset_shift_num_bits:0
    | None -> advance t ~skip ~offset_shift:0 ~offset_shift_num_bits:0
  ;;
end

let find t a b =
  if a < 0 || b <= a then invalid_arg "Parsexp.Positions.find";
  let iter = Iterator.create t in
  try
    let start_pos = Iterator.advance_exn iter ~skip:a in
    let last_pos = Iterator.advance_exn iter ~skip:(b - a - 1) in
    make_range_incl ~start_pos ~last_pos
  with
  | Iterator.No_more -> failwith "Parsexp.Position.find"
;;

let rec sub_sexp_count (sexp : Sexp.t) =
  match sexp with
  | Atom _ -> 1
  | List l -> List.fold_left l ~init:1 ~f:(fun acc x -> acc + sub_sexp_count x)
;;

module Sexp_search = struct
  exception Found of int

  let rec loop ~sub index (sexp : Sexp.t) =
    if sexp == sub
    then raise_notrace (Found index)
    else (
      match sexp with
      | Atom _ -> index + 2
      | List l ->
        let index = loop_list ~sub (index + 1) l in
        index + 1)

  and loop_list ~sub index (sexps : Sexp.t list) =
    List.fold_left sexps ~init:index ~f:(loop ~sub)
  ;;

  let finalize t ~sub a =
    let b = a + (sub_sexp_count sub * 2) - 1 in
    Some (find t a b)
  ;;

  let find_sub_sexp_phys t sexp ~sub =
    match loop ~sub 0 sexp with
    | (_ : int) -> None
    | exception Found n -> finalize t ~sub n
  ;;

  let find_sub_sexp_in_list_phys t sexps ~sub =
    match loop_list ~sub 0 sexps with
    | (_ : int) -> None
    | exception Found n -> finalize t ~sub n
  ;;
end

let find_sub_sexp_phys = Sexp_search.find_sub_sexp_phys
let find_sub_sexp_in_list_phys = Sexp_search.find_sub_sexp_in_list_phys

let to_list t =
  let iter = Iterator.create t in
  let rec loop acc =
    match Iterator.advance_exn iter ~skip:0 with
    | exception Iterator.No_more -> List.rev acc
    | pos -> loop (pos :: acc)
  in
  loop []
;;

let to_array t = to_list t |> Array.of_list
let compare t1 t2 = Caml.compare (to_array t1) (to_array t2)
let sexp_of_t t = sexp_of_array sexp_of_pos (to_array t)
