open Core.Std

let compute_max_widths header rows =
  let to_lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(to_lengths header)
    ~f:(fun acc row -> List.map2_exn ~f:Int.max acc (to_lengths row))

let render_separator widths =
  let pieces = List.map widths
    ~f:(fun width -> String.make (width + 2) '-')
  in
  "|" ^ String.concat ~sep:"+" pieces ^ "|"




let add_separators ~inner row =
  String.concat ["|";String.concat ~sep:inner row;"|"]

let pad s length =
  if String.length s >= length then s
  else s ^ String.make (length - String.length s) ' '

let render_row widths row =
  add_separators ~inner:"|"
    (List.map2_exn row widths ~f:(fun s len -> " " ^ pad s len ^ " "))

let render_separator widths =
  add_separators ~inner:"+"
    (List.map widths ~f:(fun width -> String.make (width + 2) '-'))

let check_input header rows =
  let num_columns = List.length header in
  List.iteri rows ~f:(fun i row ->
    if List.length row <> num_columns then
      invalid_argf
        "Wrong number of columns in row %d: found %d, expected %d"
        i (List.length row) num_columns ()
  )

let render_table header rows =
  check_input header rows;
  let widths = compute_max_widths header rows in
  String.concat ~sep:"\n"
    (render_row widths header
     :: render_separator widths
     :: List.map rows ~f:(fun row -> render_row widths row)
    )

type 'a column = string * ('a -> string)
let column header to_string = (header,to_string)

let render_columns columns rows =
  let header = List.map columns ~f:fst in
  let rows = List.map rows ~f:(fun row ->
    List.map columns ~f:(fun (_,to_string) -> to_string row))
  in
  render_table header rows

