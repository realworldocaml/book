type t =
  { base_name : string
  ; sub_lib : string option
  }

let compare t t' =
  let compare_opt cmp o o' =
    match o, o' with
    | None, None -> 0
    | None, _ -> -1
    | _, None -> 1
    | Some x, Some x' -> cmp x x'
  in
  let {base_name; sub_lib} = t in
  let {base_name = base_name'; sub_lib = sub_lib'} = t' in
  match String.compare base_name base_name' with
  | 0 -> compare_opt String.compare sub_lib sub_lib'
  | c -> c

let equal t t' = compare t t' = 0

let pp fmt {base_name; sub_lib} =
  let cst s = Fmt.(const string s) in
  Fmt.string fmt "{ ";
  Fmt.(pair ~sep:(cst "; ") string (option ~none:(cst "None") string)) fmt (base_name, sub_lib);
  Fmt.string fmt " }"

let from_string s =
  let invalid () = Error (Printf.sprintf "Invalid library name: %S" s) in
  match Astring.String.cuts ~sep:"." s with
  | [""] | [""; _] | [_; ""] -> invalid ()
  | [base_name] -> Ok {base_name; sub_lib = None}
  | [base_name; sl] -> Ok {base_name; sub_lib = Some sl}
  | _ -> invalid ()

module Set = Set.Make(struct
    type nonrec t = t
    let compare = compare
  end)
