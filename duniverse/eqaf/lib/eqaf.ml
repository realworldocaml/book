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
