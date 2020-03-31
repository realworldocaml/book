(* This script generates a [dune] file containing test rules. *)

(* The rules perform both "real" and "ideal" tests. *)

(* A real test uses real input and produces real output (the semantic actions)
   are executed. The code back-end and the table back-end (with and
   without the inspection API) are tested in this way. *)

(* An ideal test uses a sequence of symbolic tokens as input and produces a
   trace. The reference interpreter is tested in this way. *)

(* These tests use the parser of the calc demo. *)

(* -------------------------------------------------------------------------- *)

(* An S-expression printer. *)

type sexp =
  | A of string
  | L of sexp list

let rec print_sexp ppf = function
  | A s ->
      Format.pp_print_string ppf s
  | L l ->
      Format.fprintf ppf "@[<1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print_sexp) l

let print_sexp sexp =
  Format.printf "@[<v>%a@;@]" print_sexp sexp

(* -------------------------------------------------------------------------- *)

(* The input and reference files are stored in the subdirectory "data". *)

let data_dir =
  "data"

let input base =
  Filename.concat data_dir (base ^ ".in")

let reference base suffix =
  Filename.concat data_dir (base ^ ".ref." ^ suffix)

(* The output files are stored in the current directory,
   because dune currently does not allow storing targets
   elsewhere. *)

let output base mode suffix =
  base ^ "." ^ mode ^ "." ^ suffix

(* -------------------------------------------------------------------------- *)

(* This rule compares the results produced by Menhir in mode [mode] on the
   test [base] versus the expected results. *)

let diff base mode suffix =
  L [ A "diff"; A (reference base suffix); A (output base mode suffix) ]

let check mode base =
  assert (
    mode = "code" || mode = "table" || mode = "inspection" ||
    mode = "interpret"
  );
  print_sexp (
    L[A"rule";
      L[A"alias"; A"test"];
      L[A"action";
        L[A"progn";
          diff base mode "out";
          diff base mode "err";
  ]]])

(* -------------------------------------------------------------------------- *)

(* Real tests. *)

(* This rule runs Menhir in mode [mode] on the test [base]. *)

let exec mode base =
  print_sexp (
    L[A"rule";
      L[A"with-stdout-to"; A(output base mode "out");
      L[A"with-stderr-to"; A(output base mode "err");
      L[A"with-stdin-from"; A(input base);
      L[A"run"; A("calc/" ^ mode ^ "/calc.exe")
  ]]]]])

(* These rules takes care of the real test [base]. *)

let process_real_test base =
  exec "code" base;
  exec "table" base;
  exec "inspection" base;
  check "code" base;
  check "table" base;
  check "inspection" base

(* -------------------------------------------------------------------------- *)

(* Ideal tests. *)

(* This rule runs the reference interpreter on the test [base]. *)

let interpret base =
  print_sexp (
    L[A"rule";
      L[A"with-stdout-to"; A(output base "interpret" "out");
      L[A"with-stderr-to"; A(output base "interpret" "err");
      L[A"with-stdin-from"; A(input base);
      L[A"run"; A"menhir"; A"--trace"; A"--interpret"; A"%{dep:calc/parser.mly}"]
  ]]]])

(* These rules take care of the ideal test [base]. *)

let process_ideal_test base =
  interpret base;
  check "interpret" base

(* -------------------------------------------------------------------------- *)

(* The main program processes every file whose name is of the form *.in in the
   directory [data]. We expect every such file to be named either *.real.in or
   *.ideal.in. *)

let process base =
  let base = Filename.chop_suffix base ".in" in
  if Filename.check_suffix base ".real" then
    process_real_test base
  else if Filename.check_suffix base ".ideal" then
    process_ideal_test base
  else
    failwith ("Unexpected suffix: " ^ base)

let () =
  print_endline
    ";; This file has been auto-generated. Please do not edit it.\n\
     ;; Instead, edit [test.ml] and run [make depend].\n"

let () =
  Sys.readdir data_dir
  |> Array.to_list
  |> List.sort compare (* for determinism *)
  |> List.filter (fun base -> Filename.check_suffix base ".in")
  |> List.iter process
