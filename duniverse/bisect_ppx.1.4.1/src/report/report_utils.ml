(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



let url = "https://github.com/aantron/bisect_ppx"

module Infix =
struct
  let (++) x y =
    if ((x > 0) && (y > 0) && (x > max_int - y)) then
      max_int
    else if ((x < 0) && (y < 0) && (x < min_int - y)) then
      min_int
    else
      x + y

  let (--) x y =
    if y > min_int then
      (++) x (-y)
    else
      let res = (++) x max_int in
      if res < max_int then
        succ res
      else
        res

  let rec zip op x y =
    let lx = Array.length x in
    let ly = Array.length y in
    if lx >= ly then begin
      let z = Array.copy x in
      for i = 0 to (pred ly) do
        z.(i) <- op x.(i) y.(i)
      done;
      z
    end else
      zip op y x

  let (+|) x y = zip (++) x y

  let (-|) x y = zip (--) x y
end

let mkdirs ?(perm=0o755) dir =
  let rec mk dir =
    if not (Sys.file_exists dir) then begin
      mk (Filename.dirname dir);
      Unix.mkdir dir perm
    end in
  mk dir

let split p l =
  let rec spl acc l =
    match l with
    | hd :: tl ->
        if (p hd) then
          spl (hd :: acc) tl
        else
          (List.rev acc), l
    | [] -> (List.rev acc), [] in
  spl [] l

let split_after n l =
  let rec spl n acc l =
    match l with
    | hd :: tl ->
        if n > 0 then
          spl (pred n) (hd :: acc) tl
        else
          (List.rev acc), l
    | [] -> (List.rev acc), [] in
  spl n [] l

let open_both in_file out_file =
  let in_channel = open_in in_file in

  try
    let rec make_out_dir path =
      if Sys.file_exists path then
        ()
      else begin
        let parent = Filename.dirname path in
        make_out_dir parent;
        Unix.mkdir path 0o755
      end
    in
    make_out_dir (Filename.dirname out_file);

    let out_channel = open_out out_file in
    (in_channel, out_channel)

  with e ->
    close_in_noerr in_channel;
    raise e

let output_strings lines mapping ch =
  let get x =
    try List.assoc x mapping with Not_found -> "" in
  List.iter
    (fun l ->
      let buff = Buffer.create (String.length l) in
      Buffer.add_substitute buff get l;
      output_string ch (Buffer.contents buff);
      output_char ch '\n')
    lines

let output_bytes data filename =
  Bisect.Common.try_out_channel
    true
    filename
    (fun channel -> Array.iter (output_byte channel) data)

let current_time () =
  let now = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%d-%02d-%02d %02d:%02d:%02d"
    (1900 + now.Unix.tm_year)
    (succ now.Unix.tm_mon)
    now.Unix.tm_mday
    now.Unix.tm_hour
    now.Unix.tm_min
    now.Unix.tm_sec

type counts = { mutable visited : int; mutable total : int }

let make () = { visited = 0; total = 0 }

let update counts v =
  let open Infix in
  counts.total <- counts.total ++ 1;
  if v then counts.visited <- counts.visited ++ 1

let add counts_1 counts_2 =
  let open Infix in
  {visited = counts_1.visited ++ counts_2.visited;
   total = counts_1.total ++ counts_2.total}

let sum = List.fold_left add (make ())
