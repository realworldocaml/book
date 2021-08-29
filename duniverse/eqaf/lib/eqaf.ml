let[@inline always] char_chr ch =
  (* Char.chr contains a branch on [ch] and a plt indirection, this
   * implementation ensures well-formedness by construction and avoids that: *)
  Char.unsafe_chr (ch land 0xff)

let[@inline] get x i = String.unsafe_get x i |> Char.code

(* XXX(dinosaure): we use [unsafe_get] to avoid jump to exception:

        sarq    $1, %rbx
        movzbq  (%rax,%rbx), %rax
        leaq    1(%rax,%rax), %rax
        ret
*)

external unsafe_get_int16 : string -> int -> int = "%caml_string_get16u"
let[@inline] get16 x i = unsafe_get_int16 x i

(* XXX(dinosaure): same as [unsafe_get] but for [int16]:

        sarq    $1, %rbx
        movzwq  (%rax,%rbx), %rax
        leaq    1(%rax,%rax), %rax
        ret
*)

let equal ~ln a b =
  let l1 = ln asr 1 in

  (*
        sarq    $1, %rcx
        orq     $1, %rcx
  *)

  let r = ref 0 in

  (*
        movq    $1, %rdx
  *)

  for i = 0 to pred l1 do r := !r lor (get16 a (i * 2) lxor get16 b (i * 2)) done ;

  (*
        movq    $1, %rsi
        addq    $-2, %rcx
        cmpq    %rcx, %rsi
        jg      .L104
.L105:
        leaq    -1(%rsi,%rsi), %r8

        sarq    $1, %r8
        movzwq  (%rdi,%r8), %r9
        leaq    1(%r9,%r9), %r9
        movzwq  (%rbx,%r8), %r8
        leaq    1(%r8,%r8), %r8

     // [unsafe_get_int16 a i] and [unsafe_get_int6 b i]

        xorq    %r9, %r8
        orq     $1, %r8
        orq     %r8, %rdx
        movq    %rsi, %r8
        addq    $2, %rsi
        cmpq    %rcx, %r8
        jne     .L105
.L104:
  *)

  for _ = 1 to ln land 1 do r := !r lor (get a (ln - 1) lxor get b (ln - 1)) done ;

  (*
        movq    $3, %rsi
        movq    %rax, %rcx
        andq    $3, %rcx
        cmpq    %rcx, %rsi
        jg      .L102
.L103:
        movq    %rax, %r8
        addq    $-2, %r8

        sarq    $1, %r8
        movzbq  (%rdi,%r8), %r9
        leaq    1(%r9,%r9), %r9
        movzbq  (%rbx,%r8), %r8
        leaq    1(%r8,%r8), %r8

     // [unsafe_get a i] and [unsafe_get b i]

        xorq    %r9, %r8
        orq     $1, %r8
        orq     %r8, %rdx
        movq    %rsi, %r8
        addq    $2, %rsi
        cmpq    %rcx, %r8
        jne     .L103
.L102:
  *)

  !r = 0

(*
        cmpq    $1, %rdx
        sete    %al
        movzbq  %al, %rax
        leaq    1(%rax,%rax), %rax
        ret
*)

let equal a b =
  let al = String.length a in
  let bl = String.length b in
  if al <> bl
  then false
  else equal ~ln:al a b

let[@inline always] compare (a:int) b = a - b
let[@inline always] sixteen_if_minus_one_or_less n = (n asr Sys.int_size) land 16
let[@inline always] eight_if_one_or_more n = ((-n) asr Sys.int_size) land 8

let compare_le ~ln a b =
  let r = ref 0 in
  let i = ref (pred ln) in

  while !i >= 0 do
    let xa = get a !i and xb = get b !i in
    let c = compare xa xb in
    r := !r lor ((sixteen_if_minus_one_or_less c + eight_if_one_or_more c) lsr !r) ;
    decr i ;
  done ;

  (!r land 8) - (!r land 16)

let compare_le_with_len ~len:ln a b =
  let al = String.length a in
  let bl = String.length b in
  if ln = 0 then 0
  else if (al lxor ln) lor (bl lxor ln) <> 0
  then invalid_arg "compare_le_with_len"
  else compare_le ~ln a b

let compare_le a b =
  let al = String.length a in
  let bl = String.length b in
  if al < bl
  then 1
  else if al > bl
  then (-1)
  else compare_le ~ln:al (* = bl *) a b

let compare_be ~ln a b =
  let r = ref 0 in
  let i = ref 0 in

  while !i < ln do
    let xa = get a !i and xb = get b !i in
    let c = compare xa xb in
    r := !r lor ((sixteen_if_minus_one_or_less c + eight_if_one_or_more c) lsr !r) ;
    incr i ;
  done ;

  (!r land 8) - (!r land 16)

let compare_be_with_len ~len:ln a b =
  let al = String.length a in
  let bl = String.length b in
  if ln = 0 then 0
  else if (al lxor ln) lor (bl lxor ln) <> 0
  then invalid_arg "compare_be_with_len"
  else compare_be ~ln a b

let compare_be a b =
  let al = String.length a in
  let bl = String.length b in
  if al < bl then 1
  else if al > bl then (-1)
  else compare_be ~ln:al (* = bl *) a b

let[@inline always] minus_one_or_less n =
  n lsr (Sys.int_size - 1)

let[@inline always] one_if_not_zero n =
  minus_one_or_less ((- n) lor n)

let[@inline always] zero_if_not_zero n =
  (one_if_not_zero n) - 1

let[@inline always] select_int choose_b a b =
  let mask = ((- choose_b) lor choose_b) asr Sys.int_size in
  (a land (lnot mask)) lor (b land mask)

external int_of_bool : bool -> int = "%identity"
external unsafe_bool_of_int : int -> bool = "%identity"

let[@inline] bool_of_int n =
  unsafe_bool_of_int (one_if_not_zero n)

let[@inline always] find_uint8 ~off ~len ~f str =
  let i = ref (len - 1) in
  let a = ref (lnot 0) in
  while !i >= off do
    let byte = get str !i in
    let pred = int_of_bool (f byte) in
    (* XXX(dinosaure): a composition of [f] with [bool_of_int] such as
       [let f = bool_of_int <.> f in] implies an allocation (of a closure).
       To be GC-free, we must store result of [f] into a register, and apply
       [bool_of_int] then (introspection was done on OCaml 4.08.1). *)
    a := select_int (((!i - off) land min_int) lor pred) !a !i ;
    decr i ;
  done ; !a

let find_uint8 ?(off= 0) ~f str =
  (* XXX(dinosaure): with this overload, OCaml is able to produce 2 [find_uint8].
     One with [off= 0] and one other where [off] is an argument. I think it's about
     cross-module optimization where a call to [find_uint8 ~f v] will directly call
     the first one and a call to [find_uint8 ~off:x ~f v] will call the second one. *)
  let len = String.length str in
  find_uint8 ~off ~len ~f str

let exists_uint8 ?off ~f str =
  let v = find_uint8 ?off ~f str in
  let r = select_int (v + 1) 0 1 in
  unsafe_bool_of_int r

let divmod ~(x:int32) ~(m:int32) : int32 * int32 =
  (* Division and remainder being constant-time with respect to [x]
   * ( NOT [m] !). The OCaml variant would be:
   * [(x / m , x mod m)] where [x] is a secret and [m] is not secret.
   * Adapted from the NTRU Prime team's algorithm from
   * supercop/crypto_kem/sntrup761/ref/uint32.c
   * cite the round-2 ntru prime submission to nistpqc (march 2019)
   * Note that in practice this works for at least some much larger [x] and [m],
   * but it's unclear to me how to evaluate *which*, so leaving the original
   * restrictions in.
  *)
  let ( - ) , ( + ), ( * ) = Int32.(sub, add, mul) in
  let ( >> ) = Int32.shift_right_logical in
  if (m <= 0l) then raise (Invalid_argument "m <= 0") ;
  if (m >= 16348l) then raise (Invalid_argument "m >= 16348 not supported") ;

  let of_uint32 uint =
    (* apparently Int64.of_int32 sign-extends ... great... avoid that: *)
    let b = Bytes.make 8 '\x00' in
    Unsafe.set_int32_le b 0 uint ;
    Unsafe.get_int64_le b 0
  in

  let x_0 = x in

  let x_2, q_1 =
    let int32_div_unsigned n d =
      (* can be replaced by Int32.unsigned_div
       * from OCaml >= 4.10 *)
      let sub,min_int = Int32.(sub,min_int)in
      let int32_unsigned_compare n m =
        Int32.compare (sub n min_int) (sub m min_int)
      in
      if d < 0_l then
        if int32_unsigned_compare n d < 0 then 0_l else 1_l
      else
        let q =
          let open Int32 in
          shift_left (Int32.div (Int32.shift_right_logical n 1) d) 1 in
        let r = sub n (Int32.mul q d) in
        if int32_unsigned_compare r d >= 0 then Int32.succ q else q
    in
    let v = int32_div_unsigned Int32.min_int m |> of_uint32 in
    (*let v = 0x80_00_00_00 / m in*) (* floored div *)
    let x_1, q_0 =
      let qpart_0 =
        let open Int64 in
        shift_right_logical (mul (of_uint32 x_0) v) 31
        |> to_int32
      in
      x_0 - (qpart_0 * m), qpart_0
    in
    let qpart_1 =
      let open Int64 in
      shift_right_logical (mul (of_uint32 x_1) v) 31
      |> to_int32 in
    x_1 - (qpart_1 * m),
    (q_0 + qpart_1 + 1l) in
  let x_3 = x_2 - m  in
  let mask = 0l - (x_3 >> 31) in
  q_1 + mask, x_3 + (Int32.logand mask m)

let ascii_of_int32 ~digits (n:int32) : string =
  (* Recursively calls [divmod n 10]; the remainder is turned into ASCII
     and the quotient is used for the next division.*)
  if digits < 0 then raise (Invalid_argument "digits < 0");
  let out = Bytes.make digits '0' in
  let rec loop x = function
    | -1 -> Bytes.unsafe_to_string out
    | idx ->
      let next, this = divmod ~x ~m:10l in
      Bytes.set out idx @@ char_chr (0x30 lor (Int32.to_int this)) ;
      loop next (pred idx)
  in loop n (pred digits)

let[@inline always] to_hex_nibble f : char =
  let a = 86 + f in
  let c = 1 + ((a - 71 * ((a land 0x10) lsr 4)) lor 0x20) in
  char_chr c

let hex_of_string rawbytes =
  String.init (2 * String.length rawbytes)
    (fun idx ->
       let byt = String.get rawbytes (idx lsr 1) |> Char.code in
       (* select which 4 bits to use, this can probably be done faster:*)
       let nib = 0xf land (byt lsr (((lnot idx) land 1) lsl 2)) in
       to_hex_nibble nib)

let hex_of_bytes rawbytes = hex_of_string (Bytes.unsafe_to_string rawbytes)

let[@inline always] select_a_if_in_range ~low ~high ~n a b =
  (* select [a] if [low <= n <= high] and [b] if [n] is out of range.*)
  (* NB: ONLY WORKS FOR [0 <= low <= high <= max_int]*)
  (* The idea being that:
     1.a) if low <=   n : (n - low)  is positive +
     1.b) if low  >   n : (n - low)  is negative -
     2.a) if   n <= high: (high - n) is positive +
     2.b) if   n  > high: (high - n) is negative -
     We OR the numbers together; we only really care about the sign bit
     which is set when negative.
     Thus both numbers are positive iff (low <= n && n <= high).
     We then select the sign bit with (land min_int) and use that to choose:
  *)
  let out_of_range = (* choose b if out of range *)
    ((n - low) lor (high - n)
     land min_int)
  in
  select_int out_of_range a b

let lowercase_ascii src =
  (* ct version of String.lowercase_ascii *)
  String.map
    ( fun ch -> let n = Char.code ch in
      (* 0x41 is 'A'; 0x5a is 'Z'; 0x20 controls case for ASCII letters *)
      select_a_if_in_range ~low:0x41 ~high:0x5a ~n (n lor 0x20) (n)
      |> char_chr
    ) src

let uppercase_ascii src =
  (* ct version of String.uppercase_ascii *)
  String.map
    ( fun ch -> let n = Char.code ch in
      (* 0x61 is 'a'; 0x7a is 'z'; 0x20 controls case for ASCII letters *)
      select_a_if_in_range ~low:0x61 ~high:0x7a ~n (n lxor 0x20) (n)
      |> char_chr
    ) src

let bytes_of_hex rawhex =
  (* hex length must be multiple of 2: *)
  let error_bitmap = ref ((String.length rawhex land 1) lsl 4) in
  let decoded =
    Bytes.init (String.length rawhex lsr 1)
      (fun idx ->
         let idx = idx lsl 1 in
         let nib idx =
           String.get rawhex idx
           |> Char.code
           |> fun n -> (* uppercase -> lowercase: *)
           select_a_if_in_range ~low:0x41 ~high:0x5a
             ~n
             (n lor 0x20) (* set case bit *)
             n (* leave as-is *)
           |> fun n -> (* now either invalid; lowercase; numeric*)
           (select_a_if_in_range ~low:0x30 ~high:0x39
              ~n
              (n - 0x30) (* numeric: subtract '0' to get [0..9] *)
              (select_a_if_in_range ~low:0x61 ~high:0x66
                 ~n
                 (* a-f: subtract 'a' and add 10 to get [10..15]: *)
                 (n - 0x61 + 10)
                 (0xff) (* invalid, ensure we set upper bits of error_bitmap *)
              )
           )
         in
         let nibf0 = nib idx
         and nib0f = nib (succ idx) in
         error_bitmap := !error_bitmap lor nibf0 lor nib0f ;
         char_chr ((nibf0 lsl 4) lor nib0f)
      )
  in
  (* if any non-nibble bits were set in !error_bitmap, decoding failed: *)
  decoded, !error_bitmap land (lnot 0xf)

let string_of_hex rawhex =
  let byt, error = bytes_of_hex rawhex in
  Bytes.unsafe_to_string byt, error
