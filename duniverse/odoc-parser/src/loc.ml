type point = { line : int; column : int }
type span = { file : string; start : point; end_ : point }
type +'a with_location = { location : span; value : 'a }

let at location value = { location; value }
let location { location; _ } = location
let value { value; _ } = value
let map f annotated = { annotated with value = f annotated.value }
let same annotated value = { annotated with value }

let span spans =
  match spans with
  | [] ->
      {
        file = "_none_";
        start = { line = 1; column = 0 };
        end_ = { line = 1; column = 0 };
      }
  | first :: spans ->
      let last = List.fold_left (fun _ span -> span) first spans in
      { file = first.file; start = first.start; end_ = last.end_ }

let nudge_start offset span =
  { span with start = { span.start with column = span.start.column + offset } }
