open Core
open Int.Replace_polymorphic_compare

type dims =
  { width : int
  ; height : int
  }

let sexp_of_dims { width; height } = sexp_of_string (sprintf "w%dh%d" width height)

let dims_invariant { width; height } =
  assert (width >= 0);
  assert (height >= 0)
;;

type valign =
  [ `Top
  | `Bottom
  | `Center
  ]
[@@deriving sexp_of]

type halign =
  [ `Left
  | `Right
  | `Center
  ]
[@@deriving sexp_of]

type t =
  | Text of Utf8_text.t
  | Fill of Uchar.t * dims
  | Hcat of t * t * dims
  | Vcat of t * t * dims
  | Ansi of string option * t * string option * dims
[@@deriving sexp_of]

let height = function
  | Text _ -> 1
  | Fill (_, d) | Hcat (_, _, d) | Vcat (_, _, d) | Ansi (_, _, _, d) -> d.height
;;

let width = function
  | Text s -> Utf8_text.width s
  | Fill (_, d) | Hcat (_, _, d) | Vcat (_, _, d) | Ansi (_, _, _, d) -> d.width
;;

let uchar_newline = Uchar.of_char '\n'

let rec invariant t =
  match t with
  | Text s -> assert (not (Utf8_text.mem s uchar_newline))
  | Fill (_, dims) -> dims_invariant dims
  | Hcat (t1, t2, dims) ->
    dims_invariant dims;
    invariant t1;
    invariant t2;
    [%test_result: int] (height t1) ~expect:dims.height;
    [%test_result: int] (height t2) ~expect:dims.height;
    [%test_result: int] (width t1 + width t2) ~expect:dims.width
  | Vcat (t1, t2, dims) ->
    dims_invariant dims;
    invariant t1;
    invariant t2;
    [%test_result: int] (width t1) ~expect:dims.width;
    [%test_result: int] (width t2) ~expect:dims.width;
    [%test_result: int] (height t1 + height t2) ~expect:dims.height
  | Ansi (_, t, _, dims) ->
    dims_invariant dims;
    invariant t;
    [%test_result: int] (width t) ~expect:dims.width;
    [%test_result: int] (height t) ~expect:dims.height
;;

let fill_generic ch ~width ~height =
  assert (width >= 0);
  assert (height >= 0);
  Fill (ch, { width; height })
;;

let fill_uchar ch ~width ~height = fill_generic ch ~width ~height
let fill ch ~width ~height = fill_generic (Uchar.of_char ch) ~width ~height
let space ~width ~height = fill ' ' ~width ~height
let nil = space ~width:0 ~height:0
let hstrut width = space ~width ~height:0
let vstrut height = space ~height ~width:0
let dims t = { width = width t; height = height t }

let halve n =
  let fst = n / 2 in
  let snd = fst + (n mod 2) in
  (* fst + snd = n. snd = either fst or fst + 1 *)
  fst, snd
;;

let ansi_escape ?prefix ?suffix t = Ansi (prefix, t, suffix, dims t)

let hpad_split ~align delta =
  let value pad = if pad = 0 then None else Some pad in
  let k above below = value above, value below in
  if delta = 0
  then k 0 0
  else (
    match align with
    | `Left -> k 0 delta
    | `Right -> k delta 0
    | `Center ->
      let a, b = halve delta in
      k a b)
;;

let hpad t ~(align : halign) delta =
  assert (delta >= 0);
  let pad_left, pad_right = hpad_split ~align delta in
  let height = height t in
  let t =
    Option.fold pad_left ~init:t ~f:(fun t delta ->
      Hcat (space ~height ~width:delta, t, { height; width = width t + delta }))
  in
  let t =
    Option.fold pad_right ~init:t ~f:(fun t delta ->
      Hcat (t, space ~height ~width:delta, { height; width = width t + delta }))
  in
  t
;;

let vpad_split ~align delta =
  let value pad = if pad = 0 then None else Some pad in
  let k above below = value above, value below in
  if delta = 0
  then k 0 0
  else (
    match align with
    | `Top -> k 0 delta
    | `Bottom -> k delta 0
    | `Center ->
      let a, b = halve delta in
      k a b)
;;

let vpad t ~align delta =
  assert (delta >= 0);
  let pad_above, pad_below = vpad_split ~align delta in
  let width = width t in
  let t =
    Option.fold pad_above ~init:t ~f:(fun t delta ->
      Vcat (space ~width ~height:delta, t, { width; height = height t + delta }))
  in
  let t =
    Option.fold pad_below ~init:t ~f:(fun t delta ->
      Vcat (t, space ~width ~height:delta, { width; height = height t + delta }))
  in
  t
;;

let max_height ts = List.fold ts ~init:0 ~f:(fun acc t -> Int.max acc (height t))
let max_width ts = List.fold ts ~init:0 ~f:(fun acc t -> Int.max acc (width t))

let valign align ts =
  let h = max_height ts in
  List.map ts ~f:(fun t -> vpad ~align t (h - height t))
;;

let halign align ts =
  let w = max_width ts in
  List.map ts ~f:(fun t -> hpad ~align t (w - width t))
;;

let hcat ?(align = `Top) ?sep ts =
  let ts = Option.fold sep ~init:ts ~f:(fun ts sep -> List.intersperse ts ~sep) in
  let ts = valign align ts in
  match ts with
  | [] -> nil
  | t :: ts ->
    List.fold ~init:t ts ~f:(fun acc t ->
      assert (height acc = height t);
      Hcat (acc, t, { height = height acc; width = width acc + width t }))
;;

let vcat ?(align = `Left) ?sep ts =
  let ts = Option.fold sep ~init:ts ~f:(fun ts sep -> List.intersperse ts ~sep) in
  let ts = halign align ts in
  match ts with
  | [] -> nil
  | t :: ts ->
    List.fold ~init:t ts ~f:(fun acc t ->
      assert (width acc = width t);
      Vcat (acc, t, { width = width acc; height = height acc + height t }))
;;

let text_of_lines lines ~align =
  match lines with
  | [ line ] -> Text line
  | _ -> lines |> List.map ~f:(fun line -> Text line) |> vcat ~align
;;

let utf8_space = Utf8_text.of_string " "

let word_wrap line ~max_width =
  Utf8_text.split line ~on:' '
  |> List.filter ~f:(Fn.non Utf8_text.is_empty)
  |> List.fold ~init:(Fqueue.empty, Fqueue.empty, 0) ~f:(fun (lines, line, len) word ->
    let n = Utf8_text.width word in
    let n' = len + 1 + n in
    if n' > max_width
    then Fqueue.enqueue lines line, Fqueue.singleton word, n
    else lines, Fqueue.enqueue line word, n')
  |> (fun (lines, line, _) -> Fqueue.enqueue lines line)
  |> Fqueue.map ~f:(fun line -> Fqueue.to_list line |> Utf8_text.concat ~sep:utf8_space)
  |> Fqueue.to_list
;;

let text ?(align = `Left) ?max_width str =
  let txt = Utf8_text.of_string str in
  let lines =
    if Utf8_text.mem txt uchar_newline then Utf8_text.split ~on:'\n' txt else [ txt ]
  in
  let lines =
    match max_width with
    | None -> lines
    | Some max_width -> List.concat_map lines ~f:(word_wrap ~max_width)
  in
  text_of_lines lines ~align
;;

(* an abstract renderer, instantiated once to compute line lengths and then again to
   actually produce a string.

   [line_length] is a number of bytes rather than a number of visible characters.  The two
   may differ in case of proper unicode [Text] or [Ansi] escape sequences. *)
let render_abstract t ~write_direct ~line_length =
  for j = 0 to height t - 1 do
    write_direct uchar_newline (line_length j) j ~num_bytes:1
  done;
  let next_i = Array.init (height t) ~f:(fun _ -> 0) in
  let add_char c j =
    let i = next_i.(j) in
    let num_bytes = Uchar.utf8_byte_length c in
    next_i.(j) <- i + num_bytes;
    write_direct c i j ~num_bytes
  in
  let write_string txt j = Utf8_text.iter txt ~f:(fun uchar -> add_char uchar j) in
  let rec aux t j_offset =
    match t with
    | Text s -> write_string s j_offset
    | Fill (ch, d) ->
      for _i = 0 to d.width - 1 do
        for j = 0 to d.height - 1 do
          add_char ch (j + j_offset)
        done
      done
    | Vcat (t1, t2, _) ->
      aux t1 j_offset;
      aux t2 (j_offset + height t1)
    | Hcat (t1, t2, _) ->
      aux t1 j_offset;
      aux t2 j_offset
    | Ansi (prefix, t, suffix, _) ->
      let vcopy s =
        Option.iter s ~f:(fun s ->
          for j = 0 to height t - 1 do
            write_string s (j + j_offset)
          done)
      in
      vcopy (Option.map ~f:Utf8_text.of_string prefix);
      aux t j_offset;
      vcopy (Option.map ~f:Utf8_text.of_string suffix)
  in
  aux t 0
;;

let line_lengths t =
  let height = height t in
  let r = Array.create ~len:height 0 in
  let write_direct c i j ~num_bytes =
    let is_whitespace =
      match Uchar.to_char c with
      | None -> false
      | Some c -> Char.is_whitespace c
    in
    if not is_whitespace then r.(j) <- Int.max r.(j) (i + num_bytes)
  in
  let line_length _ = -1 (* doesn't matter *) in
  render_abstract t ~write_direct ~line_length;
  r
;;

let poke_uchar : bytes -> Uchar.t -> pos:int -> unit =
  let buffer = Caml.Buffer.create 4 in
  fun bytes c ~pos ->
    Uutf.Buffer.add_utf_8 buffer c;
    for k = 0 to Buffer.length buffer - 1 do
      Bytes.set bytes (k + pos) (Buffer.nth buffer k)
    done;
    Buffer.clear buffer
;;

let render t =
  let height = height t in
  if height = 0
  then ""
  else (
    let line_lengths = line_lengths t in
    let line_offsets, buflen =
      let r = Array.create ~len:height 0 in
      let line_offset j = r.(j - 1) + line_lengths.(j - 1) + 1 in
      for j = 1 to height - 1 do
        r.(j) <- line_offset j
      done;
      r, line_offset height
    in
    let buf = Bytes.make buflen ' ' in
    let write_direct c i j ~num_bytes:_ =
      if Uchar.equal c uchar_newline || i < line_lengths.(j)
      then poke_uchar buf c ~pos:(i + line_offsets.(j))
    in
    let line_length j = line_lengths.(j) in
    render_abstract t ~write_direct ~line_length;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf)
;;

(* header compression *)

let rec cons x = function
  | [] -> [ x ]
  | y :: zs ->
    if height x < height y then x :: y :: zs else cons (hcat ~align:`Bottom [ x; y ]) zs
;;

let rows_of_cols cols ~sep_width =
  cols |> List.transpose_exn |> List.map ~f:(fun row -> hcat row ~sep:(hstrut sep_width))
;;

let compress_table_header ?(sep_width = 2) (`Cols cols) =
  let cols =
    List.map cols ~f:(fun (header, data, align) ->
      header, Int.max 1 (max_width data), halign align data)
  in
  let header =
    hcat
      ~align:`Bottom
      (List.fold_right cols ~init:[] ~f:(fun (header, max_width, _) stairs ->
         let rec loop stairs acc =
           let stop () = cons (vcat ~align:`Left [ header; acc ]) stairs in
           match stairs with
           | [] -> stop ()
           | x :: rest ->
             if width header + sep_width <= width acc
             then stop ()
             else
               loop
                 rest
                 (hcat
                    [ vcat
                        ~align:`Left
                        [ fill '|' ~width:1 ~height:(height x - height acc); acc ]
                    ; x
                    ])
         in
         loop stairs (vcat ~align:`Left [ text "|"; hstrut (max_width + sep_width) ])))
  in
  let rows = List.map cols ~f:(fun (_, _, data) -> data) |> rows_of_cols ~sep_width in
  `Header header, `Rows rows
;;

let table ?(sep_width = 2) (`Cols cols) =
  let cols = List.map cols ~f:(fun (data, align) -> halign align data) in
  let rows = rows_of_cols cols ~sep_width in
  `Rows rows
;;

(* Produces one of a family of unicode characters that look like

   ,--U--.
   |  U  |     with U filled in if [up] is passed,
   |  U  |          D filled in if [down] is passed,
   LLLoRRR          L filled in if [left] is passed,
   |  D  |          R filled in if [right] is passed, and
   |  D  |          o filled in if any of the above are passed.
   `--D--'
*)
let box_char ?up ?down ?left ?right () =
  let boolify = function
    | None -> false
    | Some () -> true
  in
  let up = boolify up in
  let down = boolify down in
  let left = boolify left in
  let right = boolify right in
  match up, down, left, right with
  | false, false, true, true -> Uchar.of_scalar_exn 0x2500
  | true, true, false, false -> Uchar.of_scalar_exn 0x2502
  | false, true, false, true -> Uchar.of_scalar_exn 0x250c
  | false, true, true, false -> Uchar.of_scalar_exn 0x2510
  | true, false, false, true -> Uchar.of_scalar_exn 0x2514
  | true, false, true, false -> Uchar.of_scalar_exn 0x2518
  | true, true, false, true -> Uchar.of_scalar_exn 0x251c
  | true, true, true, false -> Uchar.of_scalar_exn 0x2524
  | false, true, true, true -> Uchar.of_scalar_exn 0x252c
  | true, false, true, true -> Uchar.of_scalar_exn 0x2534
  | true, true, true, true -> Uchar.of_scalar_exn 0x253c
  | false, false, true, false -> Uchar.of_scalar_exn 0x2574
  | true, false, false, false -> Uchar.of_scalar_exn 0x2575
  | false, false, false, true -> Uchar.of_scalar_exn 0x2576
  | false, true, false, false -> Uchar.of_scalar_exn 0x2577
  | false, false, false, false -> Uchar.of_char ' '
;;

module Boxed = struct
  (* The representation of a boxed text block is a generalization of [box_char] where
     there may be more than one place where it "pokes out" on each side.  The four
     directional int lists give all such positions.

     It isn't until we call [wrap] at the very end that the final border goes around the
     whole thing.
  *)
  type nonrec t =
    { contents : t (* what goes inside the box *)
    ; ups : int list (* list of column positions: 0-indexed *)
    ; downs : int list (* list of column positions: 0-indexed *)
    ; lefts : int list (* list of row positions: 0-indexed *)
    ; rights : int list (* list of row positions: 0-indexed *)
    }
  [@@deriving sexp_of]

  let cell ?(hpadding = 1) ?(vpadding = 0) contents =
    (* add any horizontal padding *)
    let contents =
      if hpadding > 0
      then vcat ~align:`Center [ contents; hstrut (width contents + (2 * hpadding)) ]
      else contents
    in
    (* add any vertical padding *)
    let contents =
      if vpadding > 0
      then hcat ~align:`Center [ contents; vstrut (height contents + (2 * vpadding)) ]
      else contents
    in
    { contents; ups = []; downs = []; lefts = []; rights = [] }
  ;;

  let box_char ?(height = 1) ?(width = 1) ?up ?down ?left ?right () =
    fill_uchar ~height ~width (box_char ?up ?down ?left ?right ())
  ;;

  let ulcorner = box_char () ~down:() ~right:()
  let urcorner = box_char () ~down:() ~left:()
  let llcorner = box_char () ~up:() ~right:()
  let lrcorner = box_char () ~up:() ~left:()

  let hline ?(ups = []) ?(downs = []) ~width () =
    let ups = Int.Set.of_list ups in
    let downs = Int.Set.of_list downs in
    hcat
      (List.init width ~f:(fun i ->
         box_char
           ()
           ~left:()
           ~right:()
           ?up:(Option.some_if (Set.mem ups i) ())
           ?down:(Option.some_if (Set.mem downs i) ())))
  ;;

  let vline ?(lefts = []) ?(rights = []) ~height () =
    let lefts = Int.Set.of_list lefts in
    let rights = Int.Set.of_list rights in
    vcat
      (List.init height ~f:(fun i ->
         box_char
           ()
           ~up:()
           ~down:()
           ?left:(Option.some_if (Set.mem lefts i) ())
           ?right:(Option.some_if (Set.mem rights i) ())))
  ;;

  (* put a border around the whole thing *)
  let wrap { contents; ups; downs; lefts; rights } =
    let width = width contents in
    let height = height contents in
    (* all directions are opposite from the border's perspective *)
    vcat
      [ hcat [ ulcorner; hline ~downs:ups ~width (); urcorner ]
      ; hcat [ vline ~rights:lefts ~height (); contents; vline ~lefts:rights ~height () ]
      ; hcat [ llcorner; hline ~ups:downs ~width (); lrcorner ]
      ]
  ;;

  (* a helper common to vcat/hcat below to concatenate lists of "poke out" positions along
     the same dimension as that of the concatenation. *)
  let concat_frills project width_or_height ~ts ~n =
    List.init ((2 * n) - 1) ~f:Fn.id
    |> List.fold ~init:(0, []) ~f:(fun (sum, vals) i ->
      let sum, new_vals =
        if i % 2 = 0
        then (
          let t = ts.(i / 2) in
          let vals = List.map (project t) ~f:(fun j -> j + sum) in
          sum + width_or_height t.contents, vals)
        else (
          let vals = [ sum ] in
          sum + 1, vals)
      in
      sum, List.rev_append new_vals vals)
    |> snd
    |> List.rev
  ;;

  let hpad t ~align delta =
    let pad_left, pad_right = hpad_split ~align delta in
    let acc = t.contents in
    let height = height acc in
    let rec padding i ~frills ~width =
      let prepend_space ~height acc =
        if height = 0 then acc else space ~width ~height :: acc
      in
      match frills with
      | [] -> prepend_space ~height:(height - i) []
      | hd :: tl ->
        prepend_space
          ~height:(hd - i)
          (box_char ~width ~left:() ~right:() () :: padding (hd + 1) ~frills:tl ~width)
    in
    let padding ~frills ~width = vcat (padding 0 ~frills ~width) in
    let acc =
      Option.fold pad_left ~init:acc ~f:(fun acc delta ->
        Hcat
          ( padding ~frills:t.lefts ~width:delta
          , acc
          , { height; width = width acc + delta } ))
    in
    let acc =
      Option.fold pad_right ~init:acc ~f:(fun acc delta ->
        Hcat
          ( acc
          , padding ~frills:t.rights ~width:delta
          , { height; width = width acc + delta } ))
    in
    acc
  ;;

  let vcat ?(align = `Left) ts =
    if List.is_empty ts
    then cell nil
    else (
      let max_width =
        List.fold ts ~init:0 ~f:(fun acc t -> Int.max acc (width t.contents))
      in
      let ts =
        List.map ts ~f:(fun t ->
          let contents = t.contents in
          let padding = max_width - width contents in
          let contents = hpad ~align t padding in
          let shift =
            let offset =
              match align with
              | `Left -> 0
              | `Center -> fst (halve padding)
              | `Right -> padding
            in
            List.map ~f:(fun n -> offset + n)
          in
          { t with contents; ups = shift t.ups; downs = shift t.downs })
      in
      let ts = Array.of_list ts in
      let n = Array.length ts in
      let contents =
        vcat
          (List.init
             ((2 * n) - 1)
             ~f:(fun i ->
               if i % 2 = 0
               then ts.(i / 2).contents
               else (
                 let prev_t = ts.((i - 1) / 2) in
                 let next_t = ts.((i + 1) / 2) in
                 (* directions flipped for the same reason as in [wrap] *)
                 hline ~ups:prev_t.downs ~downs:next_t.ups ~width:max_width ())))
      in
      let lefts_or_rights project = concat_frills project height ~ts ~n in
      { contents
      ; ups = ts.(0).ups
      ; downs = ts.(n - 1).downs
      ; lefts = lefts_or_rights (fun t -> t.lefts)
      ; rights = lefts_or_rights (fun t -> t.rights)
      })
  ;;

  let vpad t ~align delta =
    let pad_above, pad_below = vpad_split ~align delta in
    let acc = t.contents in
    let width = width acc in
    let rec padding i ~frills ~height =
      let prepend_space ~width acc =
        if width = 0 then acc else space ~width ~height :: acc
      in
      match frills with
      | [] -> prepend_space ~width:(width - i) []
      | hd :: tl ->
        prepend_space
          ~width:(hd - i)
          (box_char ~height ~up:() ~down:() () :: padding (hd + 1) ~frills:tl ~height)
    in
    let padding ~frills ~height = hcat (padding 0 ~frills ~height) in
    let acc =
      Option.fold pad_above ~init:acc ~f:(fun acc delta ->
        Vcat
          ( padding ~frills:t.ups ~height:delta
          , acc
          , { width; height = height acc + delta } ))
    in
    let acc =
      Option.fold pad_below ~init:acc ~f:(fun acc delta ->
        Vcat
          ( acc
          , padding ~frills:t.downs ~height:delta
          , { width; height = height acc + delta } ))
    in
    acc
  ;;

  let hcat ?(align = `Top) ts =
    if List.is_empty ts
    then cell nil
    else (
      let max_height =
        List.fold ts ~init:0 ~f:(fun acc t -> Int.max acc (height t.contents))
      in
      let ts =
        List.map ts ~f:(fun t ->
          let contents = t.contents in
          let padding = max_height - height contents in
          let contents = vpad ~align t padding in
          let shift =
            let offset =
              match align with
              | `Top -> 0
              | `Center -> fst (halve padding)
              | `Bottom -> padding
            in
            List.map ~f:(fun n -> offset + n)
          in
          { t with contents; lefts = shift t.lefts; rights = shift t.rights })
      in
      let ts = Array.of_list ts in
      let n = Array.length ts in
      let contents =
        hcat
          (List.init
             ((2 * n) - 1)
             ~f:(fun i ->
               if i % 2 = 0
               then ts.(i / 2).contents
               else (
                 let prev_t = ts.((i - 1) / 2) in
                 let next_t = ts.((i + 1) / 2) in
                 (* directions flipped for the same reason as in [wrap] *)
                 vline ~lefts:prev_t.rights ~rights:next_t.lefts ~height:max_height ())))
      in
      let ups_or_downs project = concat_frills project width ~ts ~n in
      { contents
      ; lefts = ts.(0).lefts
      ; rights = ts.(n - 1).rights
      ; ups = ups_or_downs (fun t -> t.ups)
      ; downs = ups_or_downs (fun t -> t.downs)
      })
  ;;
end

let boxed = Boxed.wrap

(* convenience definitions *)

let vsep = vstrut 1
let hsep = hstrut 1
let indent ?(n = 2) t = hcat [ hstrut n; t ]
let sexp sexp_of_a a = sexp_of_a a |> Sexp.to_string_hum |> text
let textf ?align ?max_width fmt = ksprintf (text ?align ?max_width) fmt

module List_with_static_lengths = struct
  type ('a, 'shape) t =
    | [] : (_, [ `nil ]) t
    | ( :: ) : 'a * ('a, 'shape) t -> ('a, [ `cons of 'shape ]) t

  let rec to_list : type a shape. (a, shape) t -> a list = function
    | [] -> []
    | hd :: tl -> hd :: to_list tl
  ;;

  let rec of_same_length_list_exn : type a shape. (a, shape) t -> a list -> (a, shape) t =
    fun t list ->
      match t with
      | [] ->
        if not (List.is_empty list) then failwith "list is too long";
        []
      | _ :: t_tl ->
        (match list with
         | [] -> failwith "list is too short"
         | list_hd :: list_tl -> list_hd :: of_same_length_list_exn t_tl list_tl)
  ;;
end

module With_static_lengths = struct
  let make align alignment static_length_list =
    List_with_static_lengths.of_same_length_list_exn
      static_length_list
      (align alignment (List_with_static_lengths.to_list static_length_list))
  ;;

  let halign h = make halign h
  let valign v = make valign v

  module List = List_with_static_lengths
end
