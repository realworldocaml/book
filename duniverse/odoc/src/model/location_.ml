type point = {
  line : int;
  column : int;
}

type span = {
  file : string;
  start : point;
  end_ : point;
}

type +'a with_location = {
  location : span;
  value : 'a;
}

let at location value =
  {location; value}

let location {location; _} =
  location

let value {value; _} =
  value

let map f annotated =
  {annotated with value = f annotated.value}

let same annotated value =
  {annotated with value}

let span spans =
  match spans with
  | [] ->
    {
      file = "_none_";
      start = {
        line = 1;
        column = 0;
      };
      end_ = {
        line = 1;
        column = 0;
      };
    }
  | first::spans ->
    let last = List.fold_left (fun _ span -> span) first spans in
    {
      file = first.file;
      start = first.start;
      end_ = last.end_;
    }

let nudge_start offset span =
  {span with start = {span.start with column = span.start.column + offset}}

let set_end_as_offset_from_start offset span =
  {span with end_ = {span.start with column = span.start.column + offset}}

let point_in_string s offset point =
  let rec scan_string line column index =
    if index >= offset then
      (line, column)
    else if index >= String.length s then
      (line, column)
    else
      match s.[index] with
      | '\n' -> scan_string (line + 1) 0 (index + 1)
      | _ -> scan_string line (column + 1) (index + 1)
  in

  let line, column = scan_string 0 0 0 in

  {line = point.line + line; column = point.column + column}

(* Calling this repeatedly on the same string can be optimized, but there is no
   evidence yet that performance of this is a problem. *)
let in_string s ~offset ~length s_span =
  {s_span with
    start = point_in_string s offset s_span.start;
    end_ = point_in_string s (offset + length) s_span.start}
