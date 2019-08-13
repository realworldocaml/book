let latex_of_string s =
  let tokens = Caml2html.Input.string s in
  let buf = Buffer.create 1000 in
  Caml2html.Output_latex.ocaml buf tokens;
  Buffer.contents buf

let print_ocaml s =
  print "\\begin{alltt}";
  print (latex_of_string s);
  print "\\end{alltt}"

(* Validate ATD syntax before printing *)
let print_atd s =
  try 
    ignore (Atd.Util.load_string
              ~expand:true ~keep_poly:true
              ~inherit_fields:true ~inherit_variants:true s);
    print_ocaml s
  with e ->
    let msg =
      match e with
          Failure s
        | Atd.Ast.Atd_error s -> s
        | _ -> Printexc.to_string e
    in
    Printf.eprintf "\
*** Invalid ATD ***
%s
*** Error ***
%s

%!"
      s msg;
    raise e

let ocaml () =
  Camlmix.print_with print_ocaml

let atd () =
  Camlmix.print_with print_atd



let read_command_output f s =
  let ic = Unix.open_process_in s in
  (try
     while true do
       f (input_char ic)
     done
   with End_of_file -> ());
  match Unix.close_process_in ic with
      Unix.WEXITED 0 -> ()
    | _ -> invalid_arg ("read_command_output: " ^ s)


let shell s =
  let buf = Buffer.create 100 in
  read_command_output (Buffer.add_char buf) s;
  print "\\begin{verbatim}";
  print (Buffer.contents buf); (* no escaping! *)
  print "\\end{verbatim}"


let odoc_url = 
  "http://oss.wink.com/atd/atd-" ^ version ^ "/odoc/index.html"
