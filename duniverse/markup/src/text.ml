open Common

type t =
  {mutable strings  : string list;
   buffer           : Buffer.t;
   mutable location : location option}

(* This is changed for unit testing. *)
let length_limit = ref (Sys.max_string_length / 2)

let prepare () = {strings = []; buffer = Buffer.create 256; location = None}

let note_location text location =
  begin match text.location with
  | None -> text.location <- Some location
  | Some _ -> ()
  end

let adding text location =
  note_location text location;

  if Buffer.length text.buffer >= !length_limit then begin
    text.strings <- (Buffer.contents text.buffer)::text.strings;
    Buffer.clear text.buffer
  end

let add text location c =
  adding text location;
  add_utf_8 text.buffer c

(* This is only used for strings that are expected to be very small, at the
   moment. *)
let add_string text location s =
  adding text location;
  Buffer.add_string text.buffer s

let emit text =
  match text.location with
  | None -> None
  | Some location ->
    text.location <- None;
    if Buffer.length text.buffer = 0 then None
    else begin
      let strings = (Buffer.contents text.buffer)::text.strings |> List.rev in
      text.strings <- [];
      Buffer.clear text.buffer;
      Some (location, strings)
    end
