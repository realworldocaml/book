let sprintf = Printf.sprintf
let ksprintf = Printf.ksprintf
open Ppx_sexp_conv_lib.Conv

module Time : sig
  type t
  val now : unit -> t
  val to_string : t -> string
  val to_string_sec : t -> string
  val to_string_abs : t -> string
end = struct
  type t = string
  let now () = "Time.now ()"
  let to_string t = "[Time.to_string (" ^ t ^ ")]"
  let to_string_sec t = "[Time.to_string_sec (" ^ t ^ ")]"
  let to_string_abs t = "[Time.to_string_abs (" ^ t ^ ")]"
end

module Zone : sig
  type t
  val local : t
  val to_string : t -> string
end = struct
  type t = string
  let local = "Zone.local"
  let to_string t = "[Zone.to_string " ^ t ^ "]"
end

let%test _ = sprintf !"The time is %{Time} and the timezone is %{Zone}.\n"
  (Time.now ()) Zone.local
  = "The time is [Time.to_string (Time.now ())] and the timezone is \
     [Zone.to_string Zone.local].\n"

(* check that custom directives with nothing in between are properly translated *)
let%test _ = sprintf !"%{sexp:int}%{sexp:int}%{sexp:int}%{sexp:int}" 1 2 3 4 = "1234"

(* check that things works well when the conversion function take optional arguments *)
let%test _ =
  let to_string ?foo:_ x = string_of_int x in
  sprintf !"%{to_string}\n" 42 = "42\n"
;;

(* check the X#y kinds of format and that arguments are not
   reversed somehow *)
let%test _ =
  let now = Time.now () in
  sprintf !"%{Time}, %{Time#sec}, %{Time.to_string_abs}\n%!" now now now
  = "[Time.to_string (Time.now ())], [Time.to_string_sec (Time.now ())], \
     [Time.to_string_abs (Time.now ())]\n"

(* same as above, with empty module paths *)
let%test _ =
  let open Time in
  let now = now () in
  sprintf !"%{}, %{#sec}, %{to_string_abs}\n%!" now now now
  = "[Time.to_string (Time.now ())], [Time.to_string_sec (Time.now ())], \
     [Time.to_string_abs (Time.now ())]\n"

(* testing what happens is the expression to the left of the format string
   is a bit complicated *)
let%test _ =
  let s = ksprintf (fun s ->
    (s ^ " foo")
  ) !"%{Time} bar" (Time.now ())
  in
  s = "[Time.to_string (Time.now ())] bar foo"

(* checking sexp: format *)
let%test "sexp conversion" =
  sprintf !"The pair is: %{sexp:int * string}" (4,"asdf")
  = "The pair is: (4 asdf)"

(* checking sexp#mach: format *)
let%test "sexp#mach conversion" =
  let module Ppx_sexp_conv_lib = struct
    module Sexp = struct
      include Ppx_sexp_conv_lib.Sexp
      let to_string_mach sexp = to_string sexp ^ " (in machine format)"
    end
  end
  in
  sprintf !"The pair is: %{sexp#mach:int * string}" (4,"asdf")
  = "The pair is: (4 asdf) (in machine format)"

(* checking tricky formats *)
let%test _ =
  sprintf !"%d %%{foo" 3
  = "3 %{foo"

let%test _ =
  sprintf !"%d %%{%{Time}" 3 (Time.now ())
  = "3 %{[Time.to_string (Time.now ())]"

(* checking that when we eta expand, we do not change side effects *)
let%test _ =
  let side_effect1_happened = ref false in
  let side_effect2_happened = ref false in
  let _f : Zone.t -> string =
    (side_effect1_happened := true; sprintf) !"%{Time} %{Zone}"
      (side_effect2_happened := true; Time.now ())
  in
  !side_effect1_happened && !side_effect2_happened

let%test _ =
  let to_string () = "plop" in
  sprintf !"%{  }" () = "plop"

let%test_unit _ =
  let f ~labeled_arg:() fmt = ksprintf (fun _ -> ()) fmt in
  (* Check that it compiles with the labeled argument applied both before and after the
     format string *)
  f ~labeled_arg:() !"hello";
  f !"hello" ~labeled_arg:()
;;

let%test_unit _ =
  let after1 = Some () in
  let f ~before:() fmt = ksprintf (fun _ ?after1:_ () ~after2:() -> ()) fmt in
  f ~before:() ?after1 !"hello" () ~after2:();
  f ~before:() !"hello" ?after1 () ~after2:();
  f !"hello" ~before:() ?after1 () ~after2:();
  f !"hello" ?after1 ~before:() () ~after2:()
;;

let%test_unit _ =
  let f ~label:() fmt = ksprintf (fun _ -> ()) fmt in
  let r = ref 0 in
  let g = f !"%{Time}" ~label:(incr r) in
  g (Time.now ()); g (Time.now ());
  assert (!r = 1)
;;

let%test "format subst" =
  sprintf !"%(%d%)" "[%d]" 1 = "[1]"
;;

let first_class_format1 = !"u = %{sexp:int * int}"
let first_class_format2 = !"t = %{Time}"
let first_class_format3 =
  first_class_format1 ^^ ", " ^^ first_class_format2 ^^ !", v = %{sexp:int}"
;;

let%test _ =
  sprintf first_class_format1 (0, 42) = "u = (0 42)"
;;
let%test _ =
  sprintf first_class_format2 (Time.now ()) = "t = [Time.to_string (Time.now ())]"
;;
let%test _ =
  sprintf first_class_format3 (0, 42) (Time.now ()) 10
  = "u = (0 42), t = [Time.to_string (Time.now ())], v = 10"
;;



