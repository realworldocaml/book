(*
  A fairly complete demonstration of the features provided
  by Easy-format.
*)


open Easy_format


let list =
  { list with
      list_style = Some "list";
      opening_style = Some "op";
      body_style = Some "body";
      separator_style = Some "sep";
      closing_style = Some "cl"
  }
let atom = { atom_style = Some "atom" }
let label = { label with label_style = Some "label" }



let tuple_param =
  { list with
      space_after_opening = false;
      space_before_closing = false;
      align_closing = false
  }

let operator_param =
  { list with
      space_after_opening = false;
      space_before_closing = false;
      separators_stick_left = false;
      space_before_separator = true;
      space_after_separator = true;
      align_closing = true
  }


let html_escape_string s =
  let buf = Buffer.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
	'&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let html_escape = `Escape_string html_escape_string
let html_style = [
  "atom", { tag_open = "<a>"; tag_close = "</a>" };
  "body", { tag_open = "<lb>"; tag_close = "</lb>" };
  "list", { tag_open = "<l>"; tag_close = "</l>" };
  "op", { tag_open = "<op>"; tag_close = "</op>" };
  "cl", { tag_open = "<cl>"; tag_close = "</cl>" };
  "sep", { tag_open = "<sep>"; tag_close = "</sep>" };
  "label", { tag_open = "<la>"; tag_close = "</la>" };
]



let format_tuple f l =
  List (("(", ",", ")", tuple_param), List.map f l)

let format_int x =
  Atom (string_of_int x, atom)

let format_float x =
  Atom (Printf.sprintf "%.5f" x, atom)

let format_sum ?(wrap = `Wrap_atoms) l =
  List (("(", "+", ")", { operator_param with wrap_body = wrap }),
	List.map format_int l)

let format_array ~align_closing ~wrap f a =
  let l = Array.to_list (Array.map f a) in
  List (("[|", ";", "|]",
	 { list with
	     align_closing = align_closing;
	     wrap_body = wrap }),
	l)

let format_matrix
    ?(align_closing1 = true)
    ?(align_closing2 = true)
    ?(wrap1 = `Wrap_atoms)
    ?(wrap2 = `Wrap_atoms)
    m =
  format_array ~align_closing: align_closing1 ~wrap: wrap1
    (format_array ~align_closing: align_closing2 ~wrap: wrap2 format_float) m


let format_record f l0 =
  let l =
    List.map
      (fun (s, x) -> Label ((Atom (s ^ ":", atom), label), f x))
      l0 in
  List (("{", ";", "}", list), l)

let begin_style =
  { label with indent_after_label = 0 },
  ("begin", ";", "end",
   { list with stick_to_label = false })

let curly_style =
  label,
  ("{", ";", "}", list)

let format_function_definition (body_label, body_param) name param body =
  Label (
    (
      Label (
	(Atom ("function " ^ name, atom), label),
	List (("(", ",", ")", tuple_param),
	      List.map (fun s -> Atom (s, atom)) param)
      ),
      body_label
    ),
    List (body_param, List.map (fun s -> Atom (s, atom)) body)
  )

(*
   Illustrate the difference between `Force_break and `Force_breaks_rec on
   labels.
*)
let label_one_atom = Atom ("reallyLongLabelOne", atom)
let label_two_atom = Atom ("reallyLongLabelTwo", atom)
let label_three_atom = Atom ("reallyLongLabelABC", atom)
let make_list_in_labels (wrap) =
  Label (
    (label_one_atom, label),
    (
      Label (
        (label_two_atom, label),
        (
          Label (
            (label_three_atom, label),
            List (
              ("[", ",", "]", { list with wrap_body = wrap }),
              [
                Atom ("1.23456", atom);
                Atom ("9.87654", atom);
                Atom ("9.87654", atom);
                Atom ("9.87654", atom);
              ]
            )
          )
        )
      )
    )
  )

(*
   Illustrate the difference between `Force_break and `Force_breaks_rec
*)
let make_heterogenous_list (container_wrap, wrap) =
  List (
    ("[", ",", "]", { list with wrap_body = container_wrap }),
    [
      Atom ("0", atom);
      List (
        ("[", ",", "]", { list with wrap_body = wrap }),
        [
          Atom ("1.23456", atom);
          Atom ("9.87654", atom);
        ]
      );
      Atom ("1", atom);
      Atom ("2", atom);
      Atom ("3", atom);
    ]
  )

let print_margin fmt () =
  let margin = Format.pp_get_margin fmt () in
  for i = 1 to margin do
    print_char '+'
  done;
  print_newline ()


let with_margin ?(html = false) margin f x =
  let fmt = Format.formatter_of_out_channel stdout in
  Format.pp_set_margin fmt margin;
  if html then
    Pretty.define_styles fmt html_escape html_style;
  print_margin fmt ();
  f fmt x;
  Format.pp_print_flush fmt ();
  print_newline ()

let print s =
  Printf.printf "\n*** %s ***\n%!" s

let print_tuple fmt l =
  Pretty.to_formatter fmt (format_tuple format_int l)

let print_heterogenous_list fmt wrap =
  Pretty.to_formatter fmt (make_heterogenous_list wrap)

let print_list_in_labels fmt wrap =
  Pretty.to_formatter fmt (make_list_in_labels wrap)

let print_sum ?wrap fmt l =
  Pretty.to_formatter fmt (format_sum ?wrap l)

let print_matrix ?align_closing1 ?align_closing2 ?wrap1 ?wrap2 m fmt () =
  Pretty.to_formatter fmt
    (format_matrix ?align_closing1 ?align_closing2 ?wrap1 ?wrap2 m)

let print_function_definition style name param fmt body =
  Pretty.to_formatter fmt (format_function_definition style name param body)

let () =
  let ints = Array.to_list (Array.init 10 (fun i -> i)) in

  (* A simple tuple that fits on one line *)
  with_margin 80 print_tuple ints;
  with_margin 20 print_tuple ints;

  (* Printed as a sum *)
  with_margin 80 print_sum ints;
  with_margin ~html:true 80 print_sum ints;
  with_margin 20 (print_sum ~wrap:`Always_wrap) ints;
  with_margin 20 (print_sum ~wrap:`Never_wrap) ints;

  (* Heterogenous list *)
  print "wrappable outer list, inner list using `Force_breaks";
  with_margin 80 print_heterogenous_list (`Always_wrap, `Force_breaks);
  with_margin 20 print_heterogenous_list (`Always_wrap, `Force_breaks);

  print "wrappable outer list, inner list using `Force_breaks_rec";
  with_margin 80 print_heterogenous_list (`Always_wrap, `Force_breaks_rec);
  with_margin 20 print_heterogenous_list (`Always_wrap, `Force_breaks_rec);

  print "never_wrap outer list, inner list using `Force_breaks";
  with_margin 80 print_heterogenous_list (`Never_wrap, `Force_breaks);
  with_margin 20 print_heterogenous_list (`Never_wrap, `Force_breaks);

  print "never_wrap outer list, inner list using `Force_breaks_rec";
  with_margin 80 print_heterogenous_list (`Never_wrap, `Force_breaks_rec);
  with_margin 20 print_heterogenous_list (`Never_wrap, `Force_breaks_rec);

  print "no breaks outer list, inner list using `Force_breaks";
  with_margin 80 print_heterogenous_list (`No_breaks, `Force_breaks);
  with_margin 20 print_heterogenous_list (`No_breaks, `Force_breaks);

  print "no breaks outer list, inner list using `Force_breaks_rec";
  with_margin 80 print_heterogenous_list (`No_breaks, `Force_breaks_rec);
  with_margin 20 print_heterogenous_list (`No_breaks, `Force_breaks_rec);

  print "label with inner list using `Force_breaks_rec";
  with_margin 80 print_list_in_labels (`Force_breaks_rec);
  with_margin 70 print_list_in_labels (`Force_breaks_rec);
  with_margin 20 print_list_in_labels (`Force_breaks_rec);

  print "label with inner list using `Force_breaks";
  with_margin 80 print_list_in_labels (`Force_breaks);
  with_margin 70 print_list_in_labels (`Force_breaks);
  with_margin 20 print_list_in_labels (`Force_breaks);

  print "label with inner list using `Never_wrap";
  with_margin 80 print_list_in_labels (`Never_wrap);
  with_margin 70 print_list_in_labels (`Never_wrap);
  with_margin 20 print_list_in_labels (`Never_wrap);

  (* Triangular array of arrays showing wrapping of lists of atoms *)
  let m = Array.init 20 (fun i -> Array.init i (fun i -> sqrt (float i))) in

  (* Default style *)
  print "default style";
  with_margin 80 (print_matrix m) ();

  (* Other styles *)
  print "style 1";
  with_margin 80 (print_matrix
		    ~align_closing1: false ~align_closing2: false m) ();
  print "style 2";
  with_margin 80 (print_matrix
		    ~align_closing1: false ~align_closing2: false
		    ~wrap2: `Never_wrap m) ();
  print "style 3";
  with_margin 80 (print_matrix
		    ~align_closing1: false ~align_closing2: false
		    ~wrap2: `Always_wrap m) ();
  print "style 4";
  with_margin 80 (print_matrix
		    ~align_closing2: false
		    ~wrap1: `Always_wrap ~wrap2: `Always_wrap m) ();
  print "style 5";
  with_margin 80 (print_matrix
		    ~align_closing1: false
		    ~wrap1: `Always_wrap ~wrap2: `Always_wrap m) ();
  print "style 6";
  with_margin 80 (print_matrix ~wrap2: `Force_breaks m) ();
  print "style 7";
  with_margin 80 (print_matrix ~wrap1: `Always_wrap ~wrap2: `No_breaks m) ();
  print "style 8";
  with_margin 80 (print_matrix ~wrap2: `No_breaks m) ();
  print "style 9";
  with_margin 80 (print_matrix ~wrap1: `No_breaks m) ();
  print "style 10";
  with_margin 80 (print_matrix ~wrap1: `No_breaks ~wrap2: `Force_breaks m) ();
  print "style 11";
  with_margin 80 (print_matrix ~wrap2: `Never_wrap m) ();


  (* A function definition, showed with different right-margin settings
     and either begin-end or { } around the function body. *)
  let program html margin style =
    with_margin ~html margin
      (print_function_definition
	 style
	 "hello" ["arg1";"arg2";"arg3"])
      [
	"print \"hello\"";
	"return (1 < 2)"
      ]
  in
  List.iter (
    fun style ->
      List.iter (
	fun margin ->
	  program false margin style;
	  program true margin style
      ) [ 10; 20; 30; 40; 80 ]
  ) [ curly_style; begin_style ]
