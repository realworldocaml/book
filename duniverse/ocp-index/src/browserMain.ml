open Lwt.Infix
open Lwt_react
open CamomileLibraryDefault.Camomile

(* LibIndex.info contains lazy values, we need a specialized equality. *)
let rec eq l1 l2 = match l1, l2 with
  | [], [] -> true
  | [] , _::_  | _::_ , [] -> false
  | {LibIndex. path = path1 ; name = name1 } :: t1 ,
    {LibIndex. path = path2 ; name = name2 } :: t2 ->
      path1 = path2 && name1 = name2 && eq t1 t2

(* * Provide an association LibIndex.kind -> tag (= string) -> style
    In order to encode styles in [Format.tag]. *)
let kind_to_tag, tag_to_style, register_ressource =
  let h = Hashtbl.create 11 in
  let kind_to_tag = function
#if OCAML_VERSION >= "4.03"
    | LibIndex.OpenType
#endif
    | LibIndex.Type -> "Type"
    | Value -> "Value"
    | Exception -> "Exception"
    | Field _  -> "Field"
    | Variant _ -> "Variant"
    | Method _ -> "Method"
    | Module -> "Module"
    | ModuleType -> "ModuleType"
    | Class -> "Class"
    | ClassType -> "ClassType"
    | Keyword -> "Keyword"
  in
  let tag_to_style s =
    try Hashtbl.find h s with Not_found -> LTerm_style.none
  in
  let register_ressource res s default =
    match res with
    | Some res ->
        let res_style = LTerm_resources.get_style s res in
        if res_style = LTerm_style.none then
          Hashtbl.add h s default
        else Hashtbl.add h s res_style
    | None ->
        Hashtbl.add h s default
  in
  kind_to_tag, tag_to_style, register_ressource

(** Load custom styles from ~/.ocp-browser. *)
let load_style () =
  let bold = LTerm_style.({ none with bold = Some true}) in
  let underline = LTerm_style.({ none with underline = Some true}) in
  let colindex i = LTerm_style.({ none with foreground = Some (index i)}) in
  let fn = Filename.concat LTerm_resources.home ".ocp-browser" in
  Lwt.(
    catch
      (fun () -> LTerm_resources.load fn >>= fun x -> Lwt.return (Some x))
      (fun _ -> Lwt.return None)
    >>= fun res ->
    register_ressource res "Type"       @@ colindex 6 ;
    register_ressource res "Value"      @@ bold       ;
    register_ressource res "Exception"  @@ colindex 3 ;
    register_ressource res "Field"      @@ colindex 4 ;
    register_ressource res "Variant"    @@ colindex 4 ;
    register_ressource res "Method"     @@ bold       ;
    register_ressource res "Module"     @@ colindex 1 ;
    register_ressource res "ModuleType" @@ colindex 1 ;
    register_ressource res "Class"      @@ colindex 5 ;
    register_ressource res "ClassType"  @@ colindex 5 ;
    register_ressource res "Keyword"    @@ colindex 7 ;
    register_ressource res "Enabled"    @@ underline  ;
    register_ressource res "Disabled"   @@ colindex 8 ;
    Lwt.return ()
    )

(** Similar to {!LTerm_text.pp_with_style} but with no typing restriction. *)
let pp_with_style to_style =
  fun style fstr fmt ->
    let tag = to_style style in
    Format.pp_open_tag fmt tag;
    Format.kfprintf
      (fun fmt ->
         Format.pp_close_tag fmt ())
      fmt fstr

let colorise opts =
  if not opts.IndexOptions.color then
    LibIndex.Format.no_color
  else
    let f kind fstr fmt = pp_with_style kind_to_tag kind fstr fmt
    in { LibIndex.Format.f }

(** Format the complete answer and return a styled text. *)
let sprint_answer ?(extra_info=false) cols colorise id =
  let get_content, fmt =
    LTerm_text.make_formatter ~read_color:tag_to_style () in
  Format.pp_set_margin fmt cols ;
  let print = Format.fprintf fmt in

  print "@[<hv 4>" ;
  LibIndex.Format.kind ~colorise fmt id;
  print " ";
  LibIndex.Format.path ~short:true ~colorise fmt id;
  begin match id with
    | { LibIndex.ty = None }
    | { LibIndex.kind = LibIndex.Module | LibIndex.ModuleType |
                        LibIndex.Class | LibIndex.ClassType }
      -> ()
    | { LibIndex.ty = Some _ } ->
        print "@ @[<h>" ;
        LibIndex.Format.ty ~colorise fmt id;
        print "@]" ;
  end ;
  print "@]" ;
  if extra_info && Lazy.force id.LibIndex.doc <> None
  then begin
    print "@\n    " ;
    LibIndex.Format.doc ~colorise fmt id
  end ;
  Format.pp_print_flush fmt () ;
  get_content ()


(** Key Bindings *)

module Bindings = Zed_input.Make (LTerm_key)

let load_bindings () =
  LTerm_inputrc.load () >>= fun () ->
  let open LTerm_read_line in
  let open LTerm_key in
  let edit x = Edit (LTerm_edit.Zed x) in
  let (~/) x = Char (UChar.of_char x) in
  bind [{ control = false; meta = false; shift = false; code = Right }]
    [edit Next_char];
  bind [{ control = false; meta = false; shift = false; code = Left }]
    [edit Prev_char];
  bind [{ control = false; meta = true; shift = false; code = Backspace }]
    [edit Delete_prev_word];

  bind [{ control = false; meta = true; shift = false; code = Home }]
    [Complete_bar_first];
  bind [{ control = false; meta = true; shift = false; code = End }]
    [Complete_bar_last];
  bind [{ control = false; meta = false; shift = false; code = Up }]
    [Complete_bar_prev];
  bind [{ control = true; meta = false; shift = false; code = ~/'p'}]
    [Complete_bar_prev];
  bind [{ control = false; meta = false; shift = false; code = Down }]
    [Complete_bar_next];
  bind [{ control = true; meta = false; shift = false; code = ~/'n'}]
    [Complete_bar_next];

  bind [{ control = false; meta = true; shift = false; code = Up }]
    [Complete_bar_prev];
  bind [{ control = false; meta = true; shift = false; code = Down }]
    [Complete_bar_next];
  bind [{ control = false; meta = true; shift = false; code = Right }]
    [Complete_bar];
  (* bind [{ control = false; meta = true; shift = false; code = Left }]
     [edit Delete_prev_word]; *)
  (* Not defined here, because what we want is not exactly a command. *)

  bind [{ control = false; meta = false; shift = false; code = Enter }]
    [Complete_bar];

  (* We use Alt+c to toggle constructors. *)
  LTerm_edit.unbind [
    { control = false ; meta = true ; shift = false ; code = ~/'c'}
  ] ;
  Lwt.return ()



(** Line editor *)

(* Delicate mix between LTerm_read_line.engine and LTerm_edit.edit *)
(* Should go into lambda-term. *)
let newline = UChar.of_char '\n'


class virtual line_editor = object(self)
  inherit LTerm_widget.t "edit"
  inherit [Zed_rope.t] LTerm_read_line.engine () as super

  method text = Zed_rope.to_string (Zed_edit.text self#edit)

  val mutable style = LTerm_style.none
  val mutable marked_style = LTerm_style.none
  val mutable current_line_style = LTerm_style.none
  method! update_resources =
    let rc = self#resource_class and resources = self#resources in
    style <- LTerm_resources.get_style rc resources;
    marked_style <- LTerm_resources.get_style (rc ^ ".marked") resources;
    current_line_style <-
      LTerm_resources.get_style (rc ^ ".current-line") resources

  val mutable event = E.never
  val mutable resolver = None

  method is_valid_char (_c : UChar.t) = true

  method! can_focus = true

  initializer
    event <-
      E.map (fun _ -> self#queue_draw)
        (Zed_edit.update self#edit [Zed_edit.cursor self#context]);
    self#on_event
      (function
        | LTerm_event.Key key -> begin
            let res =
              match resolver with
              | Some res -> res
              | None -> Bindings.resolver [
                  Bindings.pack (fun x -> x) !LTerm_read_line.bindings;
                  Bindings.pack (List.map (fun x -> LTerm_read_line.Edit x))
                    !LTerm_edit.bindings
                ]
            in
            match Bindings.resolve key res with
            | Bindings.Accepted actions ->
                resolver <- None;
                List.iter self#send_action actions ;
                true
            | Bindings.Continue res ->
                resolver <- Some res;
                true
            | Bindings.Rejected ->
                if resolver = None then
                  match key with
                  | { control = false; meta = false;
                      shift = false; code = Char ch } ->
                      Zed_macro.add self#macro
                        (Edit (LTerm_edit.Zed (Zed_edit.Insert ch)));
                      let b = self#is_valid_char ch in
                      if b then self#insert ch ;
                      b
                  | _ ->
                      false
                else begin
                  resolver <- None;
                  false
                end
          end
        | _ ->
            false)

  method! send_action = function
    | Edit (Zed Newline) -> ()
    | action -> super#send_action action

  val mutable shift = 0
  val mutable start = 0

  method! draw ctx _focused =
    let open LTerm_draw in

    let size = LTerm_draw.size ctx in

    (*** Check that the cursor is displayed ***)

    let line_set = Zed_edit.lines self#edit in
    let cursor_offset =
      Zed_cursor.get_position (Zed_edit.cursor self#context) in
    let cursor_line = Zed_lines.line_index line_set cursor_offset in
    let cursor_column =
      cursor_offset - Zed_lines.line_start line_set cursor_line in

    (* Horizontal check *)
    if cursor_column < shift || cursor_column >= shift + size.cols then
      shift <- max 0 (cursor_column - size.cols / 2);

    (* Vertical check *)
    let start_line = Zed_lines.line_index line_set start in
    let start_line =
      if cursor_line < start_line || cursor_line >= start_line + size.rows
      then begin
        let start_line = max 0 (cursor_line - size.rows / 2) in
        start <- Zed_lines.line_start line_set start_line;
        start_line
      end
      else start_line
    in

    (*** Drawing ***)

    (* Initialises points with the text style and spaces. *)
    fill ctx (UChar.of_char ' ');
    fill_style ctx style;

    (*** Text drawing ***)

    let rec draw_line row col zip =
      if Zed_rope.Zip.at_eos zip then
        draw_eoi (row + 1)
      else
        let char, zip = Zed_rope.Zip.next zip in
        if char = newline then begin
          let row = row + 1 in
          if row < size.rows then begin_line row zip
        end else begin
          if col > size.cols then begin
            let row = row + 1 in
            if row < size.rows then skip_eol row zip
          end else begin
            draw_char ctx row col char;
            draw_line row (col + 1) zip
          end
        end

    and skip_eol row zip =
      if Zed_rope.Zip.at_eos zip then
        draw_eoi (row + 1)
      else
        let char, zip = Zed_rope.Zip.next zip in
        if char = newline then
          begin_line row zip
        else
          skip_eol row zip

    and skip_bol row zip remaining =
      if remaining = 0 then
        draw_line row 0 zip
      else if Zed_rope.Zip.at_eos zip then
        draw_eoi (row + 1)
      else
        let char, zip = Zed_rope.Zip.next zip in
        if char = newline then begin
          let row = row + 1 in
          if row < size.rows then begin_line row zip
        end else
          skip_bol row zip (remaining - 1)

    and begin_line row zip =
      if Zed_rope.Zip.at_eos zip then
        draw_eoi row
      else if shift <> 0 then begin
        skip_bol row zip shift
      end else
        draw_line row 0 zip

    and draw_eoi _row =
      ()
    in

    let text = Zed_edit.text self#edit in

    begin_line 0 (Zed_rope.Zip.make_f text start);

    (* Colorize the current line. *)
    for col = 0 to size.cols - 1 do
      set_style (point ctx (cursor_line - start_line) col) current_line_style
    done;

    (* Colorize the selection if needed *)
    if Zed_edit.get_selection self#edit then begin
      let sel_offset = Zed_cursor.get_position (Zed_edit.mark self#edit) in
      let sel_line = Zed_lines.line_index line_set sel_offset in
      let sel_column = sel_offset - Zed_lines.line_start line_set sel_line in
      let line_a, column_a, line_b, column_b =
        if sel_offset < cursor_offset then
          (sel_line, sel_column, cursor_line, cursor_column)
        else
          (cursor_line, cursor_column, sel_line, sel_column)
      in
      let line_a, column_a =
        if line_a < start_line then
          (start_line, 0)
        else
          (line_a, column_a)
      in
      let line_b, column_b =
        if line_b >= start_line + size.rows then
          (start_line + size.rows - 1, size.cols - 1)
        else
          (line_b, column_b)
      in
      if line_a < start_line + size.rows && line_b >= start_line then begin
        let line_a = line_a - start_line and line_b = line_b - start_line in
        let column_a = column_a and column_b = column_b in
        if line_a = line_b then
          for column = column_a to column_b - 1 do
            set_style (point ctx line_a column) marked_style
          done
        else begin
          for column = column_a to size.cols - 1 do
            set_style (point ctx line_a column) marked_style
          done;
          for line = line_a + 1 to line_b - 1 do
            for column = 0 to size.cols - 1 do
              set_style (point ctx line column) marked_style
            done
          done;
          for column = 0 to column_b - 1 do
            set_style (point ctx line_b column) marked_style
          done
        end
      end
    end

  method! cursor_position =
    let line_set = Zed_edit.lines self#edit in
    let cursor_offset =
      Zed_cursor.get_position (Zed_edit.cursor self#context) in
    let cursor_line = Zed_lines.line_index line_set cursor_offset in
    let cursor_column =
      cursor_offset - Zed_lines.line_start line_set cursor_line in
    let start_line = Zed_lines.line_index line_set start in
    Some { row = cursor_line - start_line; col = cursor_column - shift }
end


(* * Strip one path level.

    Do the following transformation:
    "Foo.Bar."    -> "Foo."
    "Foo.Bar.bla" -> "Foo.Bar."

    Since some library use "_" as namespace marker, we stop at "_" too.
*)
(* Currently, it computes where to cut, go to the end of the text,
   erase the extra text, replace the cursor.
   It's not atomic and will fire multiple useless events.
*)
let strip_path_level text context =
  if Zed_rope.is_empty text then ()
  else begin
    let module Z = Zed_rope.Zip in
    let dot = UChar.of_char '.' in
    let underscore = UChar.of_char '_' in
    (* If the last char is a dot, we want to skip it, otherwise, we don't care.*)
    let zip = Z.make_b text 1 in
    let i = Z.(offset (find_b ( fun x -> x = dot || x = underscore) zip)) in
    let len = Zed_rope.length text in
    let previous_pos = Zed_edit.position context in

    Zed_edit.goto_eot context ;
    Zed_edit.remove_prev context (len - i) ;
    Zed_edit.goto context (min i previous_pos)
  end

let index_of_biggest_prefix s l =
  let len_s = Zed_utf8.length s in
  let rec loop k len_acc n_acc = function
    | [] -> n_acc
    | (h,_)::t -> begin
        if Zed_utf8.starts_with s h then
          let len_h = Zed_utf8.length h in
          if len_h = len_s then Some k (* We won't find bigger. *)
          else if len_h > len_acc then loop (k + 1) len_h (Some k) t
          else loop (k + 1) len_acc n_acc t
        else loop (k + 1) len_acc n_acc t
      end
  in loop 0 min_int None l

(* Filter out module names of the form "Foo__bar" when Foo exists. *)
module SSet = Set.Make(String)
let filter_completion l =
  let re_double_undescore = Re.(compile @@ str "__") in
  let aux (s,l) ({LibIndex. name ; kind } as h) =
    match kind with
    | Module ->
        begin match Re.split re_double_undescore name with
          | base_name :: _ when SSet.mem base_name s -> (s, l)
          | _ -> (SSet.add name s, h::l)
          | exception Not_found -> (SSet.add name s, h::l)
        end
    | _ ->
        (s, h::l)
  in
  List.rev @@ snd @@ List.fold_left aux (SSet.empty, []) l

(* Sort the list of completions. *)
let sort_completion l =
  let cmp
      {LibIndex. file = f1; loc_impl = l1}
      {LibIndex. file = f2; loc_impl = l2} =
    let name_of_file (LibIndex.Cmi s | Cmt s | Cmti s) = s in
    let i = String.compare (name_of_file f1) (name_of_file f2) in
    if i <> 0 then i
    else
      compare (Lazy.force l1).loc_start (Lazy.force l2).loc_start
  in
  List.sort cmp l

(** Mono line input with completion for a LibIndex.path. *)
class completion_box options exit =

  let completion_info, set_completion_info =
    S.create ~eq ([] : LibIndex.info list) in

  object (self)
    inherit line_editor as super

    val size_request = { LTerm_geom. rows = 1; cols = 1 }
    method! size_request = size_request

    method eval = Zed_edit.text self#edit

    initializer
      self#on_event
      (function
        | LTerm_event.Key { control = false; meta = true; shift = false; code = Left } ->
            strip_path_level (Zed_edit.text self#edit) self#context ;
            true
        | _ -> false
      )

    (** Switch for disabling/enabling all the non-modules kinds on Alt+a. *)
    val mutable module_state = false
    method module_state = module_state
    method set_module_state b =
      module_state <- b

    (* We maintain the last item under the cursor in order to try
       to restore a "good" index after completion. *)
    val mutable previous_completion = S.const ""
    initializer
      let r = ref "" in
      previous_completion <-
        S.fold (fun _ (x,_) -> let x' = !r in r := x ; x') "" @@
          S.changes @@ S.l2
          (fun l i -> try List.nth l i with _ -> ("",""))
          self#completion_words self#completion_index

    method! completion =
      let content = self#eval in
      let response =
        LibIndex.complete
          options.IndexOptions.lib_info
          ~filter:(IndexOptions.filter options)
          (Zed_rope.to_string content)
        |> sort_completion
        |> filter_completion
      in
      set_completion_info response ;
      let completions =
        let suffix = function
          | {LibIndex. kind = Module | ModuleType } -> "."
          | {LibIndex. kind = Class | ClassType } -> "#"
          | _ -> ""
        in
        List.map
          (fun x -> LibIndex.Print.path ~short:true x, suffix x)
          response
      in

      let prev_comp = S.value previous_completion in
      let index = index_of_biggest_prefix prev_comp completions in
      self#set_completion ?index 0 completions

    method completion_info = completion_info

    (** Only insert chars that are valid in OCaml identifiers. *)
    method! is_valid_char  x = try match UChar.char_of x with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '\'' | '#'
      | '!'|'$'|'%'|'&'|'*'|'+'|'-'|'.'|'/'
      | ':'|'<'|'='|'>'|'?'|'@'|'^'|'|'|'~'
        -> true
      | _ -> false
      with UChar.Out_of_range -> false

    method! send_action = function
      (* Exit the app on Break and Interrupt *)
      | action ->
          try super#send_action action
          with Sys.Break | LTerm_read_line.Interrupt -> exit ()

  end


(* Count the size took by a text. *)
let size (str : LTerm_text.t) =
  let last = Array.length str - 1 in
  let rows = ref 0 in
  let cols = ref 0 in
  let current_col = ref 0 in
  for i = 0 to last do
    if fst str.(i) = newline then begin
      incr rows ;
      cols := max !cols !current_col ;
      current_col := 0 ;
    end
    else
      incr current_col
  done ;
  (* Don't count a potential last newline twice *)
  if fst str.(last) <> newline then incr rows ;
  {LTerm_geom. rows = !rows ; cols = !cols }

(** The show box shows the result of a research.

    [content] is a list zipper positioned at the focused element.
    Left and right lists are elements before and after the focus.

    We want to draw a focused element, as in the middle as possible, at
    [default_pos]. We don't want to format more left and right elements than
    necessary.

    We first format the left elements, in right-to-left order, until the height
    of formatted texts is more than [default_pos]. We then format right elements
    in left-to-right order until the total height of formatted text is longer
    than the number of rows. We may need to go back to the left elements, if
    there are not enough right elements.

    We then render everything in the same fashion, with the focused element at
    [max 0 (min size_left default_pos)].
*)
class show_box color = object (self)
  inherit LTerm_widget.t "show_box"

  val mutable content = [], []

  method content = content

  method set_content new_content =
    content <- new_content ;
    self#queue_draw

  (** Number of entry currently printed. *)
  val mutable printed_entries = (0,0)
  method printed_entries = printed_entries

  val mutable extra_info = true
  method toogle_extra_info =
    extra_info <- not extra_info

  method! draw ctx _focused =
    let {LTerm_geom. rows ; cols } =
      LTerm_geom.size_of_rect self#allocation
    in
    let cols = cols - 3 in
    match content with
    | _, [] -> ()
    | left, focus :: right -> begin
        let text_focus = sprint_answer ~extra_info cols color focus in
        let size_focus = (size text_focus).rows in
        let default_pos = (rows - size_focus) / 2 in

        (* Can't figure out how to do simpler, bear with me. *)
        let rec format
            (dir:[ `L | `R ]) left right size_l size_r format_l format_r =
          match dir, left, right with
          (* We are done (no more to draw or too much drawn already). *)
          | _, [], [] -> size_l, size_r, List.rev format_l, List.rev format_r
          | _, _ , _ when size_r + size_focus + size_l > rows ->
              size_l, size_r, List.rev format_l, List.rev format_r

          (* Finished the left part, and stuff to do on the right. *)
          | `L , [], _::_ ->
              format `R left right size_l size_r format_l format_r
          | `L , _ , _::_ when size_l > default_pos ->
              format `R left right size_l size_r format_l format_r

          (* The right part is too short, go back to the left. *)
          | `R, _::_, [] ->
              format `L left right size_l size_r format_l format_r

          | `L, info :: t, _ | `R, _, info :: t ->
              let text = sprint_answer cols color info in
              let size = (size text).rows in
              if dir = `L then
                format dir t right (size + size_l) size_r
                  ((text,size) :: format_l) format_r
              else
                format dir left t size_l (size + size_r)
                  format_l ((text,size) :: format_r)
        in
        let size_left, right_size, formatted_left, formatted_right =
          format `L left right 0 0 [] []
        in

        let rec draw_left pos = function
          | [] -> ()
          | (text, size) :: t ->
              LTerm_draw.draw_styled ctx (pos-size) 3 text ;
              draw_left (pos-size) t
        in
        let rec draw_right pos = function
          | [] -> ()
          | (text, size) :: t ->
              LTerm_draw.draw_styled ctx pos 3 text ;
              draw_right (pos + size) t
        in

        printed_entries <-
          List.length formatted_left, List.length formatted_right ;

        let start =
          max 0 (min size_left
                   (max default_pos (rows - right_size - size_focus)))
        in

        draw_left start formatted_left ;
        LTerm_draw.draw_styled ctx start 3 text_focus ;
        (* XXX: This will force all .LibIndex.doc that the cursor passes ! *)
        let doc_hint_char = match Lazy.force focus.LibIndex.doc with
          | Some _ -> if extra_info then '-' else '+'
          | None -> ' ' in
        LTerm_draw.draw_char ctx start 0 @@ CamomileLibrary.UChar.of_char doc_hint_char ;
        LTerm_draw.draw_char ctx start 1 @@ CamomileLibrary.UChar.of_char '>' ;
        draw_right (start + size_focus) formatted_right
      end

end


(* Only in ocaml >= 4.02 *)
let rec pp_print_list ?(pp_sep = Format.pp_print_cut) pp_v ppf = function
  | [] -> ()
  | [v] -> pp_v ppf v
  | v :: vs ->
    pp_v ppf v;
    pp_sep ppf ();
    pp_print_list ~pp_sep pp_v ppf vs

(** Pretty printer for kinds with colors. *)
let pp_kinds fmt options =
  let pp_kind fmt (c, hash, b) =
    if b then (
      Format.pp_open_tag fmt "Enabled" ;
      pp_with_style (fun x -> x) hash "%s" fmt c ;
      Format.pp_close_tag fmt ()
    ) else
      pp_with_style (fun x -> x) "Disabled" "%s" fmt c ;
  in
  let open IndexOptions in
  let { t ; v ; e ; c ; m ; s ; k } = options.filter in
  let l = [
    ("t","Type",t) ; ("v","Value",v) ; ("e","Exception",e) ;
    ("c","Variant",c) ; ("m","Module",m) ; ("s","ModuleType",s) ;
    ("k","Keyword",k) ] in
  let pp_sep fmt () = Format.pp_print_string fmt ", " in
  Format.fprintf fmt " kinds: %a " (pp_print_list ~pp_sep pp_kind) l

(** A frame with extra info on the border. *)
class frame_info options = object
  inherit LTerm_widget.frame as super

  method! draw ctx focused =
    super#draw ctx focused ;
    let get_content, fmt =
      LTerm_text.make_formatter ~read_color:tag_to_style () in
    pp_kinds fmt options ;
    let s = get_content () in
    let width = (LTerm_draw.size ctx).cols in
    let len = Array.length s in
    if width > len + 2 then
      LTerm_draw.draw_styled ctx 0 (width - len - 1) s
end


(* The list of non-modules kinds. *)
let value_kind_char_list = List.map UChar.of_char ['t';'e';'c';'k';'v']

(* The list of modules kinds. *)
let modules_kind_char_list = List.map UChar.of_char ['m';'s']

(* The list of all kinds. *)
let kind_char_list = value_kind_char_list @ modules_kind_char_list

let event_handler (cbox : #completion_box) (sbox:#show_box) options show_help =
  function
  | LTerm_event.Key
      { control = false; meta = true; shift = false; code = Char ch }
    when ch = UChar.of_char 'a' ->
      let open IndexOptions in
      let fil = options.filter in
      (* Checks if the user didn't re-enabled/disabled everything by himself.
         Avoids silly double-toggle. *)
      let new_state =
        if cbox#module_state
        then fil.t && fil.e && fil.c && fil.k && fil.v
        else fil.t || fil.e || fil.c || fil.k || fil.v
      in
      cbox#set_module_state new_state ;
      let new_b = not new_state in
      options.filter <-
        { fil with
          t = new_b ; e = new_b ; c = new_b ; k = new_b ; v = new_b
        } ;
      cbox#completion ;
      sbox#queue_draw ;
      true
  | LTerm_event.Key
      { control = false; meta = true; shift = false; code = Char ch }
      when List.mem ch kind_char_list
    ->
      let open IndexOptions in
      let fil = options.filter in
      let new_fil = try match UChar.char_of ch with
        | 't' -> { fil with t = not fil.t }
        | 'e' -> { fil with e = not fil.e }
        | 'c' -> { fil with c = not fil.c }
        | 'm' -> { fil with m = not fil.m }
        | 's' -> { fil with s = not fil.s }
        | 'k' -> { fil with k = not fil.k }
        | 'v' -> { fil with v = not fil.v }
        | _ -> fil
        with UChar.Out_of_range -> fil
      in options.filter <- new_fil ;
      cbox#completion ;
      sbox#queue_draw ;
      true
  | LTerm_event.Key
      { control = false ; meta = false ; shift = false ; code = Prev_page } ->
      let k, _ = sbox#printed_entries in
      for _i = 0 to max 1 k do cbox#send_action Complete_bar_prev done ;
      true
  | LTerm_event.Key
      { control = false ; meta = false ; shift = false ; code = Next_page } ->
      let _, k = sbox#printed_entries in
      for _i = 0 to max 1 k do cbox#send_action Complete_bar_next done ;
      true
  | LTerm_event.Key
      { control = false ; meta = true ; shift = false ; code = Char c}
    when c = UChar.of_char 'h' ->
      show_help () ;
      true
  | LTerm_event.Key
      { control = false ; meta = false ; shift = false ; code = Char c}
    when c = UChar.of_char ' ' ->
      sbox#toogle_extra_info ;
      sbox#queue_draw ;
      true
  | _ -> false

(* * Express the result as an event mapped on the content of the completion box.
*)
let show_completion show_box input =
  let zipper n l =
    let rec aux k acc = function
      | h :: l when k > 0 -> aux (k-1) (h::acc) l
      | l -> acc, l
    in aux n [] l
  in
  let eq_pair (l1, l2) (l1', l2') = eq l1 l1' && eq l2 l2' in

  input#completion_info
  |> S.l2 ~eq:eq_pair zipper input#completion_index
  |> S.map show_box#set_content

(** Modal help **)

let help_content : (_,_,_,_) format4 = "\
Tab          : Complete.\n\
Enter        : Choose the result under the cursor.\n\
Space        : Toogle extra documentation.\n\
Up/Down      : Move the cursor up/down.\n\
Page Up/Down : Move up/down to the first non-visible entry.\n\
Alt+Arrows   : Move in the module hierarchy.\n\
Alt+<X>      : Toogle one of the%a\n\
Alt+a        : Toggle all the non-modules kinds.\n\
Alt+h        : Show this help.\n\
Ctrl+C       : Quit.\n\n\
Press anything to quit this help.\n"

class help_label options =
  let text =
    LTerm_text.styprintf ~read_color:tag_to_style
      help_content pp_kinds options
  in
  let h = size text in
  object
    inherit LTerm_widget.t "label"

    method! size_request = h

    method! draw ctx _focused =
      LTerm_draw.draw_styled ctx 0 0 text
end

let new_help_modal options =
  let m = new LTerm_widget.modal_frame in
  let label = new help_label options in
  m#set label ;
  m


(** Boilerplate *)

let main options =
  let do_run, push_layer, pop_layer, exit =
    LTerm_widget.prepare_simple_run () in

  let root = new LTerm_widget.vbox in
  let comp = new frame_info options in
  let input = new completion_box options exit in
  comp#set input ;
  root#add ~expand:false comp ;

  let show_box = new show_box (colorise options) in
  root#add show_box ;

  let help_modal = new_help_modal options in
  help_modal#on_event (fun _ -> pop_layer () ; true) ;

  root#on_event (event_handler input show_box options (push_layer help_modal)) ;

  S.keep @@ show_completion show_box input ;

  do_run root

let run options () =
  Lwt_main.run (
    load_style () >>=
    load_bindings >>=
    fun () -> main options
  )

let main_term : unit Cmdliner.Term.t * Cmdliner.Term.info =
  let open Cmdliner in
  let doc = "Interactively completes and prints documentation." in
  let man = [`S "DESCRIPTION"; `P "See alt+h for help."] in
  Term.(pure run
        $ IndexOptions.common_opts ~default_filter:[`T;`V;`E;`C;`M;`S;`K] ()
        $ pure ()),
  Term.info "ocp-browser" ~doc ~man

let () =
  match Cmdliner.Term.eval main_term
  with
  | `Error _ -> exit 1
  | _ -> exit 0
