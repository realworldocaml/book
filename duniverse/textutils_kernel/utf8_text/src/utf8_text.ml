open! Core

type t = string [@@deriving compare, sexp_of]

let fold_with_start_pos t ~init ~f =
  let require_uchar pos = function
    | `Malformed s -> raise_s [%message "Not UTF-8" ~_:(s : string) (pos : int)]
    | `Uchar uchar -> uchar
  in
  Uutf.String.fold_utf_8 (fun init pos x -> f init pos (require_uchar pos x)) init t
;;

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    fold_with_start_pos t ~init:() ~f:(fun () (_ : int) (_ : Uchar.t) -> ()))
;;

include Container.Make0 (struct
    type nonrec t = t

    module Elt = Uchar

    let fold t ~init ~f =
      fold_with_start_pos t ~init ~f:(fun init _pos uchar -> f init uchar)
    ;;

    let iter = `Define_using_fold
    let length = `Define_using_fold
  end)

let concat ?sep ts = String.concat ts ?sep
let is_empty = String.is_empty

let of_string t =
  invariant t;
  t
;;

let split str ~on =
  match on with
  | '\000' .. '\127' -> String.split str ~on
  | '\128' .. '\255' ->
    raise_s [%sexp "Utf8_text.split: can't split on a non-ascii char", (on : char)]
;;

let to_string = String.to_string


let assumed_width_per_uchar = 1
let width t = sum (module Int) t ~f:(const assumed_width_per_uchar)
let bytes = String.length
let space_uchar = Uchar.of_scalar_exn (Char.to_int ' ')

let chunks_of t ~width ~prefer_split_on_spaces =
  match t with
  | "" -> [ "" ]
  | _ ->
    let uchar_ends_before_pos =
      Uutf.String.fold_utf_8 (fun acc start_pos _ -> start_pos :: acc) [] t
      |> List.cons (String.length t)
      |> List.rev
      |> List.tl_exn
    in
    (* We identify uchars by the byte positions after their last bytes *)
    let chunks =
      match prefer_split_on_spaces with
      | false ->
        assert (assumed_width_per_uchar = 1);
        uchar_ends_before_pos |> List.chunks_of ~length:width
      | true ->
        let get_num_uchars_in_chunk =
          let ends_of_spaces =
            fold_with_start_pos t ~init:[] ~f:(fun acc start_pos uchar ->
              match Uchar.equal space_uchar uchar with
              | true -> (start_pos + assumed_width_per_uchar) :: acc
              | false -> acc)
            |> Set.of_list (module Int)
          in
          fun uchars_left ->
            List.take uchars_left width
            |> List.rev
            |> List.findi ~f:(fun _ pos -> Set.mem ends_of_spaces pos)
            |> Option.map ~f:(fun (uchars_after_last_space, _) ->
              width - uchars_after_last_space)
            |> Option.value ~default:width
        in
        let rec chunks_split_on_spaces chunks num_uchars_left = function
          | [] -> List.rev chunks
          | _ :: _ as uchars_left ->
            (match num_uchars_left * assumed_width_per_uchar <= width with
             | true -> chunks_split_on_spaces (uchars_left :: chunks) 0 []
             | false ->
               let num_uchars_in_chunk = get_num_uchars_in_chunk uchars_left in
               let chunk, rest = List.split_n uchars_left num_uchars_in_chunk in
               let num_uchars_left = num_uchars_left - num_uchars_in_chunk in
               chunks_split_on_spaces (chunk :: chunks) num_uchars_left rest)
        in
        let num_uchars = List.length uchar_ends_before_pos in
        chunks_split_on_spaces [] num_uchars uchar_ends_before_pos
    in
    let chunk_ends_before_pos = chunks |> List.map ~f:List.last_exn |> Sequence.of_list in
    chunk_ends_before_pos
    |> Sequence.unfold_with ~init:0 ~f:(fun start_at end_before ->
      Yield (String.sub t ~pos:start_at ~len:(end_before - start_at), end_before))
    |> Sequence.to_list
;;

let of_uchar_list uchars =
  let buf = Buffer.create 8 (* arbitrary small number *) in
  List.iter uchars ~f:(Uutf.Buffer.add_utf_8 buf);
  of_string (Buffer.contents buf)
;;

include
  Quickcheckable.Of_quickcheckable
    (struct
      module Uchar = struct
        type t = Uchar.t

        include
          Quickcheckable.Of_quickcheckable_filtered
            (Int)
            (struct
              type nonrec t = t

              let of_quickcheckable = Uchar.of_scalar
              let to_quickcheckable = Uchar.to_scalar
            end)
      end

      type t = Uchar.t list [@@deriving quickcheck]
    end)
    (struct
      type nonrec t = t

      let of_quickcheckable = of_uchar_list
      let to_quickcheckable = to_list
    end)

let iteri t ~f =
  ignore
    (fold t ~init:0 ~f:(fun i uchar ->
       f i uchar;
       i + 1)
     : int)
;;
