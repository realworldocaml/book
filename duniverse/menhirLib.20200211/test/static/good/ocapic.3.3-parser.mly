(* Original file: ocapic.3.3/ocapic-3.3/src/simulator/circuit/parser.mly *)
/*************************************************************************/
/*                                                                       */
/*                                OCaPIC                                 */
/*                                                                       */
/*                             Benoit Vaugon                             */
/*                                                                       */
/*    This file is distributed under the terms of the CeCILL license.    */
/*    See file ../../LICENSE-en.                                         */
/*                                                                       */
/*************************************************************************/

%{
  open Component;;
  open Window;;
  open Printf;;

  let int_of_string s =
    try int_of_string s
    with Failure _ -> failwith (sprintf "%S should be an integer" s);
  ;;

  let pin_of_string s =
    try Simul.pin_of_string s
    with Invalid_argument _ -> failwith (sprintf "%S should be a pin" s);
  ;;

  let bool_of_string s =
    try bool_of_string s
    with Invalid_argument _ -> failwith (sprintf "%S should be a boolean" s);
  ;;

  let bool_of_onoff s =
    match String.lowercase_ascii s with
      | "on" -> true
      | "off" -> false
      | _ -> failwith (sprintf "%S should be ON or OFF" s);
  ;;

  let orientation_of_string s =
    match String.lowercase_ascii s with
      | "horizontal" -> `Horizontal
      | "vertical"   -> `Vertical
      | _ -> failwith (sprintf "%S should be HORIZONTAL or VERTICAL" s);
  ;;

  let parse_options options config =
    let rec g opt =
      match opt with
        | (name, _) :: tl ->
          if List.mem_assoc name tl then
            failwith (sprintf "duplicate option %S" name);
          if not (List.mem_assoc name config) then
            failwith (sprintf "unknown option %S" name);
          g tl
        | [] -> ()
    in
    let rec f conf acc =
      match conf with
        | (name, None) :: tl ->
          let value =
            try List.assoc name options
            with Not_found ->
              failwith (sprintf "option %S required but not provided" name)
          in
          f tl ((name, value) :: acc)
        | (name, Some def) :: tl ->
          let value =
            try List.assoc name options
            with Not_found -> def
          in
          f tl ((name, value) :: acc)
        | [] -> acc
    in
    g options;
    f config []
  ;;

  let parse_component name options =
    match String.lowercase_ascii name with
      | "window" ->
        let opts = parse_options options [
          ("width", None); ("height", None);
          ("bgcolor", Some "#003278");
          ("title", Some "circuit");
        ] in
        let w = int_of_string (List.assoc "width" opts) in
        let h = int_of_string (List.assoc "height" opts) in
        let c = Color.of_string (List.assoc "bgcolor" opts) in
        let t = List.assoc "title" opts in
        set_window { win_width = w; win_height = h; win_bgcolor = c; win_title = t };
        None
      | "led" ->
        let opts = parse_options options [
          ("x", None); ("y", None);
          ("radius", Some "10"); ("color", Some "red");
          ("pin", None); ("inverse", Some "false");
        ] in
        let x = int_of_string (List.assoc "x" opts) in
        let y = int_of_string (List.assoc "y" opts) in
        let r = int_of_string (List.assoc "radius" opts) in
        let c = Color.of_string (List.assoc "color" opts) in
        let p = pin_of_string (List.assoc "pin" opts) in
        let i = bool_of_string (List.assoc "inverse" opts) in
        Some (Led { led_x = x; led_y = y; led_radius = r; led_color = c;
                    led_pin = p; led_inv = i; })
      | "bicolor_led" ->
        let opts = parse_options options [
          ("x", None); ("y", None);
          ("radius", Some "10"); ("color1", Some "red");
          ("color2", Some "green"); ("pin1", None); ("pin2", None);
          ("inverse", Some "false");
        ] in
        let x = int_of_string (List.assoc "x" opts) in
        let y = int_of_string (List.assoc "y" opts) in
        let r = int_of_string (List.assoc "radius" opts) in
        let c1 = Color.of_string (List.assoc "color1" opts) in
        let c2 = Color.of_string (List.assoc "color2" opts) in
        let p1 = pin_of_string (List.assoc "pin1" opts) in
        let p2 = pin_of_string (List.assoc "pin2" opts) in
        let i = bool_of_string (List.assoc "inverse" opts) in
        Some (Ledb { ledb_x = x; ledb_y = y; ledb_radius = r;
                     ledb_color1 = c1; ledb_color2 = c2;
                     ledb_pin1 = p1; ledb_pin2 = p2; ledb_inv = i; })
      | "button" ->
        let opts = parse_options options [
          ("x", None); ("y", None);
          ("width", Some "30"); ("height", Some "30");
          ("color", Some "white"); ("label", Some "");
          ("pin", Some ""); ("pin_in", Some ""); ("pin_out", Some "");
          ("inverse", Some "false");
        ] in
        let x = int_of_string (List.assoc "x" opts) in
        let y = int_of_string (List.assoc "y" opts) in
        let w = int_of_string (List.assoc "width" opts) in
        let h = int_of_string (List.assoc "height" opts) in
        let c = Color.of_string (List.assoc "color" opts) in
        let i = bool_of_string (List.assoc "inverse" opts) in
        let l = List.assoc "label" opts in
        let p = List.assoc "pin" opts in
        let pi = List.assoc "pin_in" opts in
        let po = List.assoc "pin_out" opts in
        let pin, pin_out = match p, pi, po with
          | "", "", "" ->
            failwith "option \"pin\" or options \"pin_in\" and \"pin_out\" \
                      required but not provided"
          | "", "", _ -> failwith "option \"pin_in\" required but not provided"
          | "", _, "" -> failwith "option \"pin_out\" required but not provided"
          | _, "", "" -> pin_of_string p, None
          | "", _, _  -> pin_of_string pi, Some (pin_of_string po)
          | _, "", _  -> failwith "incompatible options \"pin\" and \"pin_out\""
          | _, _, _   -> failwith "incompatible options \"pin\" and \"pin_in\""
        in
        Some (But { but_x = x; but_y = y; but_width = w; but_height = h;
                    but_color = c; but_label = l; but_pin = pin;
                    but_pin_out = pin_out; but_inv = i; })
      | "switch" ->
        let opts = parse_options options [
          ("x", None); ("y", None);
          ("width", Some "15"); ("height", Some "50");
          ("color", Some "blue"); ("label", Some "");
          ("pin", None); ("default", Some "off");
          ("orientation", Some "vertical");
        ] in
        let x = int_of_string (List.assoc "x" opts) in
        let y = int_of_string (List.assoc "y" opts) in
        let w = int_of_string (List.assoc "width" opts) in
        let h = int_of_string (List.assoc "height" opts) in
        let c = Color.of_string (List.assoc "color" opts) in
        let p = pin_of_string (List.assoc "pin" opts) in
        let d = bool_of_onoff (List.assoc "default" opts) in
        let l = List.assoc "label" opts in
        let o = orientation_of_string (List.assoc "orientation" opts) in
        Some (Swi { swi_x = x; swi_y = y; swi_width = w; swi_height = h;
                    swi_color = c; swi_label = l; swi_pin = p; swi_def = d;
                    swi_orient = o; })
      | "7segment" ->
        let opts = parse_options options [
          ("x", None); ("y", None);
          ("width", Some "60"); ("height", Some "75");
          ("color", Some "red"); ("bgcolor", Some "antiquewhite");
          ("a", None); ("b", None); ("c", None); ("d", None);
          ("e", None); ("f", None); ("g", None); ("p", Some "none");
          ("common", Some "none"); ("inverse", Some "false");
        ] in
        let x = int_of_string (List.assoc "x" opts) in
        let y = int_of_string (List.assoc "y" opts) in
        let w = int_of_string (List.assoc "width" opts) in
        let h = int_of_string (List.assoc "height" opts) in
        let a = pin_of_string (List.assoc "a" opts) in
        let b = pin_of_string (List.assoc "b" opts) in
        let c = pin_of_string (List.assoc "c" opts) in
        let d = pin_of_string (List.assoc "d" opts) in
        let e = pin_of_string (List.assoc "e" opts) in
        let f = pin_of_string (List.assoc "f" opts) in
        let g = pin_of_string (List.assoc "g" opts) in
        let p = List.assoc "p" opts in
        let p = if p = "none" then None else Some (pin_of_string p) in
        let m = List.assoc "common" opts in
        let m = if m = "none" then None else Some (pin_of_string m) in
        let i = bool_of_string (List.assoc "inverse" opts) in
        let d = make_seg7_disp x y w h a b c d e f g in
        let color = Color.of_string (List.assoc "color" opts) in
        let bgcolor = Color.of_string (List.assoc "bgcolor" opts) in
        Some (Seg7 { seg7_x = x; seg7_y = y; seg7_width = w;
                     seg7_height = h; seg7_disp = d; seg7_color = color;
                     seg7_bgcolor = bgcolor; seg7_point = p; seg7_common = m;
                     seg7_inv = i; })
      | "lcd" ->
        let opts = parse_options options [
          ("x", None); ("y", None);
          ("e", None); ("rs", None); ("rw", None); ("bus", None);
          ("column_nb", None); ("line_nb", None);
        ] in
        let x = int_of_string (List.assoc "x" opts) in
        let y = int_of_string (List.assoc "y" opts) in
        let e = Simul.pin_of_string (List.assoc "e" opts) in
        let rs = Simul.pin_of_string (List.assoc "rs" opts) in
        let rw = Simul.pin_of_string (List.assoc "rw" opts) in
        let bus = Simul.port_of_string (List.assoc "bus" opts) in
        let column_nb = int_of_string (List.assoc "column_nb" opts) in
        let line_nb = int_of_string (List.assoc "line_nb" opts) in
        Some (Lcd(Display.create_display x y e rs rw bus column_nb line_nb))
      | "analog" ->
        let opts = parse_options options [
          ("x", None); ("y", None);
          ("width", None); ("length", None);
          ("orientation", Some "horizontal");
          ("color", Some "white");
          ("pin", None);
          ("init", Some "");
        ] in
        let x = int_of_string (List.assoc "x" opts) in
        let y = int_of_string (List.assoc "y" opts) in
        let width = int_of_string (List.assoc "width" opts) in
        let len = int_of_string (List.assoc "length" opts) in
        let o = orientation_of_string (List.assoc "orientation" opts) in
        let color = Color.of_string (List.assoc "color" opts) in
        let an = Simul.an_of_string (List.assoc "pin" opts) in
        let init =  List.assoc "init" opts in
        let init = if init = "" then None else Some (float_of_string init) in
        if width < 3 then
          failwith ("too small width: " ^ string_of_int width);
        if len < width + 1 then
          failwith ("incomptible length/width: " ^ string_of_int width ^ "/" ^ string_of_int len);
        (match init with
        | None -> ()
        | Some init ->
          if not (init >= 0. && init <= 1.) then
            failwith ("invalid initial position (should be in range [ 0; 1 ]: " ^ string_of_float init));
        Some (Ana { ana_x = x; ana_y = y; ana_width = width; ana_length = len;
                    ana_orient = o; ana_color = color; ana_an = an; ana_init = init })
      | _ -> failwith ("unknown component: " ^ name)
  ;;
%}

%token <string> Name
%token <string> String
%token Equal Newline Eof

%start main
%type <Component.t list> main

%%

main: components Eof { $1 };

  components:
  | { [] }
  | Newline components { $2 }
  | component components { match $1 with Some c -> c :: $2 | None -> $2 }
  ;

  component: Name options Newline { parse_component $1 $2 };

  options:
  | { [] }
  | Name Equal name_string options { ($1, $3) :: $4 }
  ;

  name_string:
  | Name { $1 }
  | String { $1 }
  ;
