open Sexplib.Std

type line = S.line [@@deriving sexp]

type nd = [`Command | `Output | `False] [@@deriving sexp]

type test = {
  part: string option;
  non_deterministic: nd;
  command: string;
  output: [`Output of string | `Ellipsis | `Exit_code of int] list;
  exit_code: int;
  lines: line list;
} [@@deriving sexp]

type item =
  | Test of test
  | Line of line
[@@deriving sexp]

type t = item list [@@deriving sexp]

let is_meta s =String.length s >= 2 && String.sub s 0 2 = "@@"
let pp_line ?(hide=false) ppf line =
  let pp_meta ppf fmt =
    Fmt.kstrf (fun str ->
        if not (hide && is_meta str) then Fmt.string ppf str
      ) fmt
  in
  match line with
  | `Output s         -> Fmt.pf ppf "  %s\n" s
  | `Part s           -> Fmt.pf ppf "### %s\n" s
  | `Command s        -> Fmt.pf ppf "  $ %s\n" s
  | `Ellipsis         -> Fmt.pf ppf "  ...\n"
  | `Exit_code i      -> pp_meta ppf "exit %d" i
  | `Non_det `Output  -> pp_meta ppf "%%%% --non-deterministic\n"
  | `Non_det `Command -> pp_meta ppf "%%%% --non-deterministic [skip]\n"
  | `Comment s        -> pp_meta ppf "%s\n" s

let fold ~filename l =
  let rec output acc k = function
    | `Ellipsis:: t     -> output (`Ellipsis :: acc) k t
    | `Output s :: t    -> output (`Output s :: acc) k t
    | `Comment "" :: t  -> output (`Output "" :: acc) k t
    | `Exit_code n :: t -> k (List.rev (`Exit_code n :: acc)) n t
    | t -> k (List.rev acc) 0 t
  and command part k = function
    | []                   -> k []
    | `Comment _ as l :: t -> command part (fun ls -> k (Line l :: ls)) t
    | `Part p as l :: t    -> command (Some p) (fun ls -> k (Line l :: ls)) t
    | `Command s as l :: t -> create [l] `False part s k t
    | (`Non_det nd as d) :: (`Command s as l) :: t ->
      create [d;l] (nd :> nd) part s k t
    | (`Non_det _ | `Output _ | `Ellipsis | `Exit_code _) as x :: _ ->
      Fmt.failwith "%s: %a malformed input: %a"
        filename (fun ppf -> pp_line ppf) x Fmt.(Dump.list pp_line) l
  and create ls non_deterministic part s k t =
    output [] (fun output n rest ->
        let lines = ls @ (output :> S.line list) in
        let c = {
          lines; part;
          non_deterministic; command = s;
          output; exit_code = n
        } in
        command part (fun rest ->
            k (Test c :: rest)
          ) rest
      ) t
  in
  command None (fun x -> x) l

let parse_lexbuf l =
  Lexer.file l |> fold ~filename:l.Lexing.lex_start_p.pos_fname

let parse_file f = Lexer.file (snd (Common.init f)) |> fold ~filename:f

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

let pp ?hide ppf t =
  List.iter (function
      | Line l -> pp_line ?hide ppf l
      | Test t -> List.iter (pp_line ?hide ppf) t.lines
    ) t

let to_string ?hide t =
  Fmt.to_to_string (pp ?hide) t

let pp_exit_code ppf n = if n <> 0 then Fmt.pf ppf "%s exit %d\n" "@@" n

type output = [`Output of string | `Ellipsis | `Exit_code of int]

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

module Html = struct

  let pp_line ppf line =
    match line with
    | `Output s  -> Fmt.pf ppf ">%s\n" s
    | `Part _    -> assert false
    | `Command s -> Fmt.pf ppf "%s\n" s
    | `Ellipsis  -> Fmt.pf ppf "  ...\n"
    | `Exit_code n -> Fmt.pf ppf "[%d]\n" n
    | `Non_det _
    | `Comment _ -> ()


  let pp ppf t =
    List.iter (function
        | Line l -> pp_line ppf l
        | Test t -> List.iter (pp_line ppf) t.lines
      ) t
end

let to_html t = Fmt.to_to_string Html.pp t
