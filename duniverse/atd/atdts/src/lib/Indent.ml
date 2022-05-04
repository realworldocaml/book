(*
   Simple indentation utility for code generators

   Something similar is found in atdgen/src but this API is simpler.
*)

type node =
  | Line of string
  | Block of node list
  | Inline of node list

type t = node list

let rec is_empty_node = function
  | Line "" -> true
  | Line _ -> false
  | Block xs -> List.for_all is_empty_node xs
  | Inline xs -> List.for_all is_empty_node xs

let to_buffer ?(offset = 0) ?(indent = 2) buf l =
  let rec print n = function
    | Block l -> List.iter (print (n + indent)) l
    | Inline l -> List.iter (print n) l
    | Line "" -> Buffer.add_char buf '\n'
    | Line s ->
        for _ = 1 to n do
          Buffer.add_char buf ' '
        done;
        Buffer.add_string buf s;
        Buffer.add_char buf '\n';
  in
  List.iter (print offset) l

let to_string ?offset ?indent l =
  let buf = Buffer.create 1000 in
  to_buffer ?offset ?indent buf l;
  Buffer.contents buf

let to_channel ?offset ?indent oc l =
  let buf = Buffer.create 1000 in
  to_buffer ?offset ?indent buf l;
  Buffer.output_buffer oc buf

let to_stdout ?offset ?indent l =
  to_channel ?offset ?indent stdout l

let to_file ?indent path l =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> to_channel ?indent oc l)
