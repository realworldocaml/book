(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



type point_definition = {
    offset : int;
    identifier : int;
  }

(* Utility functions *)

let try_finally x f h =
  let res =
    try
      f x
    with e ->
      (try h x with _ -> ());
      raise e in
  (try h x with _ -> ());
  res

let try_in_channel bin x f =
  let open_ch = if bin then open_in_bin else open_in in
  try_finally (open_ch x) f (close_in_noerr)

let try_out_channel bin x f =
  let open_ch = if bin then open_out_bin else open_out in
  try_finally (open_ch x) f (close_out_noerr)


(* I/O functions *)

(* filename + reason *)
exception Invalid_file of string * string

let magic_number_rtd = "BISECTOUT3"

module Writer :
sig
  type 'a t

  val int : int t
  val string : string t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val array : 'a t -> 'a array t

  val write : 'a t -> 'a -> string
end =
struct
  type 'a t = Buffer.t -> 'a -> unit

  let w =
    Printf.bprintf

  let int b i =
    w b " %i" i

  let string b s =
    w b " %i %s" (String.length s) s

  let pair left right b (l, r) =
    left b l;
    right b r

  let array element b a =
    w b " %i" (Array.length a);
    Array.iter (element b) a

  let write writer v =
    let b = Buffer.create 4096 in
    Buffer.add_string b magic_number_rtd;
    writer b v;
    Buffer.contents b
end

module Reader :
sig
  type 'a t

  val int : int t
  val string : string t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val array : 'a t -> 'a array t

  val read : 'a t -> filename:string -> 'a
end =
struct
  type 'a t = Buffer.t -> in_channel -> 'a

  let junk c =
    try ignore (input_char c)
    with End_of_file -> ()

  let int b c =
    Buffer.clear b;
    let rec loop () =
      match input_char c with
      | exception End_of_file -> ()
      | ' ' -> ()
      | c -> Buffer.add_char b c; loop ()
    in
    loop ();
    int_of_string (Buffer.contents b)

  let string b c =
    let length = int b c in
    let s = really_input_string c length in
    junk c;
    s

  let pair left right b c =
    let l = left b c in
    let r = right b c in
    l, r

  let array element b c =
    let length = int b c in
    Array.init length (fun _index -> element b c)

  let read reader ~filename =
    try_in_channel true filename begin fun c ->
      let magic_number_in_file =
        try really_input_string c (String.length magic_number_rtd)
        with End_of_file ->
          raise
            (Invalid_file
              (filename, "unexpected end of file while reading magic number"))
      in
      if magic_number_in_file <> magic_number_rtd then
        raise (Invalid_file (filename, "bad magic number"));

      junk c;

      let b = Buffer.create 4096 in
      try reader b c
      with e ->
        raise
          (Invalid_file
            (filename, "exception reading data: " ^ Printexc.to_string e))
    end
end

let table : (string, int array * string) Hashtbl.t Lazy.t =
  lazy (Hashtbl.create 17)

let reset_counters () =
  Lazy.force table
  |> Hashtbl.iter begin fun _ (point_state, _) ->
    match Array.length point_state with
    | 0 -> ()
    | n -> Array.fill point_state 0 (n - 1) 0
  end

let runtime_data_to_string () =
  let data = Hashtbl.fold (fun k v acc -> (k, v)::acc) (Lazy.force table) [] in
  match data with
  | [] ->
    None
  | _ ->
    Array.of_list data
    |> Writer.(write (array (pair string (pair (array int) string))))
    |> fun s -> Some s

let write_runtime_data channel =
  let data =
    match runtime_data_to_string () with
    | Some s -> s
    | None -> Writer.(write (array int)) [||]
  in
  output_string channel data

let () =
  Random.self_init ()

let random_filename base_name =
  Printf.sprintf "%s%09d.coverage" base_name (abs (Random.int 1000000000))

let write_points points =
  let points_array = Array.of_list points in
  Array.sort compare points_array;
  Marshal.to_string points_array []

let get_relative_path file =
  if Filename.is_relative file then
    file
  else
    let cwd = Sys.getcwd () in
    let cwd_end = String.length cwd in
    let sep_length = String.length Filename.dir_sep in
    let sep_end = sep_length + cwd_end in
    try
      if String.sub file 0 cwd_end = cwd &&
          String.sub file cwd_end sep_length = Filename.dir_sep then
        String.sub file sep_end (String.length file - sep_end)
      else
        file
    with Invalid_argument _ ->
      file

let read_runtime_data filename =
  Reader.(read (array (pair string (pair (array int) string)))) ~filename
  |> Array.to_list
  |> List.map (fun (file, data) -> get_relative_path file, data)

let read_points s =
  let points_array : point_definition array = Marshal.from_string s 0 in
  Array.sort compare points_array;
  Array.to_list points_array

let register_file file ~point_count ~point_definitions =
  let point_state = Array.make point_count 0 in
  let table = Lazy.force table in
  if not (Hashtbl.mem table file) then
    Hashtbl.add table file (point_state, point_definitions);
  `Staged (fun point_index ->
    let current_count = point_state.(point_index) in
    point_state.(point_index) <-
      if current_count < max_int then
        current_count + 1
      else
        current_count)



let bisect_file = ref None
let bisect_silent = ref None

type options = (Arg.key * Arg.spec * Arg.doc) list

let deprecated binary basename options =
  let make make_spec fn =
    (basename,
    make_spec (fun v ->
      Printf.eprintf "%s argument '%s' is deprecated.\n" binary basename;
      Printf.eprintf "Use '-%s' instead.\n" basename;
      Printf.eprintf "This requires Bisect_ppx >= 2.0.0.\n";
      fn v),
    " Deprecated")
  in

  let (_, spec, _) =
    options |> List.find (fun (option, _, _) -> option = "-" ^ basename) in

  let deprecated_option =
    match spec with
    | Arg.Unit f -> make (fun f -> Arg.Unit f) f
    | Arg.Set r -> make (fun f -> Arg.Unit f) (fun () -> r := true)
    | Arg.String f -> make (fun f -> Arg.String f) f
    | Arg.Set_string r -> make (fun f -> Arg.String f) ((:=) r)
    | Arg.Int f -> make (fun f -> Arg.Int f) f
    | _ -> prerr_endline basename; assert false
  in

  options @ [deprecated_option]
