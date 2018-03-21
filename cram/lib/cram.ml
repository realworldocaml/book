open Sexplib.Std

type line = S.line [@@deriving sexp]

type nd = [`Command | `Output | `False] [@@deriving sexp]

type test = {
  part: string option;
  non_deterministic: nd;
  command: string;
  output: [`Output of string | `Ellipsis] list;
  lines: line list;
} [@@deriving sexp]

type item =
  | Test of test
  | Line of line
[@@deriving sexp]

type t = item list [@@deriving sexp]

let fold l =
  let rec output ls acc k = function
    | `Comment _ as l :: t -> output (l::ls) acc k t
    | `Ellipsis as l  :: t -> output (l:: t) (`Ellipsis :: acc) k t
    | `Output s as l  :: t -> output (l::ls) (`Output s :: acc) k t
    | l                    -> k (List.rev ls) (List.rev acc) l
  and command lines part k = function
    | []                   -> k (List.map (fun l -> Line l) lines)
    | `Comment _ as l :: t -> command lines part (fun ls -> k (Line l :: ls)) t
    | `Part p as l :: t    -> command lines (Some p) (fun ls -> k (Line l :: ls)) t
    | `Command s as l :: t -> create (l :: lines) `False part s k t
    | (`Non_det nd as d) :: (`Command s as l) :: t ->
      create (l :: d :: lines) (nd :> nd) part s k t
    | (`Non_det _ | `Output _ | `Ellipsis) :: _ -> failwith "malformed input"
  and create ls non_deterministic part s k t =
    output ls [] (fun lines output rest ->
        let c = { lines; part; non_deterministic; command = s; output } in
        command [] part (fun rest ->
            k (Test c :: rest)
          ) rest
      ) t
  in
  command [] None (fun x -> x) l

let parse_lexbuf l = Lexer.file l |> fold
let parse_file f = Lexer.file (snd (Common.init f)) |> fold

let run n ~f =
  Common.run_expect_test n ~f:(fun c l ->
      f c (parse_lexbuf l)
    )

let part n t =
  match
    List.filter (function
        | Line _ -> false
        | Test t -> t.part = Some n
      ) t
  with
  | [] -> None
  | l  -> Some l

let is_hidden s = String.length s >= 2 && String.sub s 0 2 = "@@"

let pp_line ?(hide=false) ppf = function
  | `Output s         -> Fmt.pf ppf "  %s\n" s
  | `Part s           -> Fmt.pf ppf "### %s\n" s
  | `Command s        -> Fmt.pf ppf "  $ %s\n" s
  | `Ellipsis         -> Fmt.pf ppf "  ...\n"
  | `Non_det `Output  -> Fmt.string ppf "%% --non-deterministic\n"
  | `Non_det `Command -> Fmt.string ppf "%% --non-deterministic [skip]\n"
  | `Comment s        ->
    if hide && is_hidden s then Fmt.string ppf "" else Fmt.pf ppf "%s\n" s

let pp ?hide ppf t =
  List.iter (function
      | Line l -> pp_line ?hide ppf l
      | Test t -> List.iter (pp_line ?hide ppf) t.lines
    ) t

let to_string ?hide t =
  Fmt.to_to_string (pp ?hide) t

let pp_exit_code ppf n = if n <> 0 then Fmt.pf ppf "%s exit %d\n" "@@" n

type output = [`Output of string | `Ellipsis]

let equal_output a b =
  let rec aux x y = match x, y with
    | [], []  | [`Ellipsis], _   | _, [`Ellipsis]  -> true
    | (`Ellipsis::a as x), (_::b as y) | (_::b as y), (`Ellipsis::a as x) ->
      aux x b || (* n+ matches: skip y's head *)
      aux a y    (* 0  match  : skip x's head *)
    | a::b, h::t -> a = h && aux b t
    | _ -> false
  in
  aux a b
