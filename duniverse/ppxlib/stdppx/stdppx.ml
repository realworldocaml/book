module Caml = Stdlib

open Caml
open StdLabels

module Sexp = Sexplib0.Sexp
module Sexpable = Sexplib0.Sexpable
include Sexplib0.Sexp_conv

module type Comparisons = sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val min : t -> t -> t
  val max : t -> t -> t
end

module Poly = struct
  let compare = compare
  let equal = ( = )
  let ( = ) = ( = )
  let ( < ) = ( < )
  let ( > ) = ( > )
  let ( <> ) = ( <> )
  let ( <= ) = ( <= )
  let ( >= ) = ( >= )
  let min = min
  let max = max
end

include (Poly : Comparisons with type t := int)

module Array = Array

module Bool = struct
  let to_string = string_of_bool

  include (Poly : Comparisons with type t := bool)
end

module Bytes = struct
  include Bytes

  let sub_string t ~pos ~len =
    Stdlib.Bytes.sub_string t pos len

  let blit_string ~src ~src_pos ~dst ~dst_pos ~len =
    Stdlib.Bytes.blit_string src src_pos dst dst_pos len
end

module Char = struct
  include Char

  include (Poly : Comparisons with type t := char)
end

module Exn = struct
  let protectx x ~f ~finally =
    match f x with
    | y -> finally x; y
    | exception exn -> finally x; raise exn
end

module Float = struct
  let to_string = string_of_float

  include (Poly : Comparisons with type t := float)
end

module Fn = struct
  let id x = x
end

module Hashtbl = struct
  include Hashtbl

  let set t ~key ~data =
    while mem t key do
      remove t key
    done;
    add t key data

  let add t ~key ~data =
    if mem t key
    then Error (Invalid_argument "Hashtbl.add_exn")
    else (add t key data; Ok ())

  let add_exn t ~key ~data =
    match add t ~key ~data with
    | Ok () -> ()
    | Error exn -> raise exn

  let find_opt t key =
    match find t key with
    | data -> Some data
    | exception Not_found -> None

  let find_or_add t key ~default =
    match find_opt t key with
    | Some data -> data
    | None ->
      let data = default () in
      add_exn t ~key ~data;
      data

  let rec add_alist t alist =
    match alist with
    | [] -> Ok ()
    | (key, data) :: tail ->
      match add t ~key ~data with
      | Ok () -> add_alist t tail
      | Error (_ : exn) -> Error key

  let of_alist ?size alist =
    let size =
      match size with
      | Some size -> size
      | None -> List.length alist
    in
    let t = create size in
    match add_alist t alist with
    | Ok () -> Ok t
    | Error _ as error -> error

  let of_alist_exn ?size alist =
    match of_alist ?size alist with
    | Ok t -> t
    | Error _ -> raise (Invalid_argument "Hashtbl.of_alist_exn")
end

module In_channel = struct
  let create ?(binary = true) file =
    let flags = [Open_rdonly] in
    let flags = if binary then Open_binary :: flags else flags in
    open_in_gen flags 0o000 file

  let with_file ?binary filename ~f =
    let t = create ?binary filename in
    Exn.protectx t ~f ~finally:close_in

  let input_all t =
    let rec read_all_into t buf =
      match input_char t with
      | char -> Buffer.add_char buf char; read_all_into t buf
      | exception End_of_file -> ()
    in
    let buf = Buffer.create 64 in
    read_all_into t buf;
    Buffer.contents buf

  let read_all filename =
    with_file filename ~f:input_all
end

module Int = struct
  let max_int = max_int

  let to_string = string_of_int

  include (Poly : Comparisons with type t := int)
end

module List = struct
  include List

  include struct (* shadow non-tail-recursive functions *)
    let merge = `not_tail_recursive
    let remove_assoc = `not_tail_recursive
    let remove_assq = `not_tail_recursive

    let rev_mapi list ~f =
      let rec rev_mapi_at list i ~f ~acc =
        match list with
        | [] -> acc
        | head :: tail -> rev_mapi_at tail (i + 1) ~f ~acc:(f i head :: acc)
      in
      rev_mapi_at list 0 ~f ~acc:[]

    let fold_right2 list1 list2 ~init ~f =
      fold_left2 (rev list1) (rev list2) ~init ~f:(fun acc x y -> f x y acc)

    let map list ~f = rev (rev_map list ~f)
    let mapi list ~f = rev (rev_mapi list ~f)

    let fold_right list ~init ~f =
      fold_left (List.rev list) ~init ~f:(fun acc x -> f x acc)

    let append x y = rev_append (rev x) y
    let concat list = fold_right list ~init:[] ~f:append

    let rev_combine list1 list2 =
      fold_left2 list1 list2 ~init:[] ~f:(fun acc x y -> (x, y) :: acc)

    let combine list1 list2 = rev (rev_combine list1 list2)

    let split list =
      fold_right list ~init:([], []) ~f:(fun (x, y) (xs, ys) -> (x :: xs, y :: ys))

    let map2 list1 list2 ~f =
      rev (fold_left2 list1 list2 ~init:[] ~f:(fun acc x y -> f x y :: acc))
  end

  let init ~len ~f =
    let rec loop ~len ~pos ~f ~acc =
      if pos >= len
      then List.rev acc
      else loop ~len ~pos:(pos + 1) ~f ~acc:(f pos :: acc)
    in
    loop ~len ~pos:0 ~f ~acc:[]

  let is_empty = function
    | [] -> true
    | _ :: _ -> false

  let rev_filter_opt list =
    fold_left list ~init:[] ~f:(fun tail option ->
      match option with
      | None -> tail
      | Some head -> head :: tail)

  let filter_opt list = rev (rev_filter_opt list)

  let filter_map list ~f = rev_filter_opt (rev_map list ~f)

  let concat_map list ~f = concat (map list ~f)

  let rec find_map list ~f =
    match list with
    | [] -> None
    | head :: tail ->
      match f head with
      | Some _ as some -> some
      | None -> find_map tail ~f

  let find_map_exn list ~f =
    match find_map list ~f with
    | Some x -> x
    | None -> raise Not_found

  let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: ((_ :: _) as rest) -> last rest

  let split_while list ~f =
    let rec split_while_into list ~f ~acc =
      match list with
      | head :: tail when f head -> split_while_into tail ~f ~acc:(head :: acc)
      | _ :: _ | [] -> List.rev acc, list
    in
    split_while_into list ~f ~acc:[]

  let find_a_dup (type elt) list ~compare =
    let module Elt = struct
      type t = elt

      let compare = compare
    end in
    let module Elt_set = Set.Make (Elt) in
    let rec find_a_dup_in list ~set =
      match list with
      | [] -> None
      | head :: tail ->
        if Elt_set.mem head set
        then Some head
        else find_a_dup_in tail ~set:(Elt_set.add head set)
    in
    find_a_dup_in list ~set:Elt_set.empty

  let assoc_opt key alist =
    match assoc key alist with
    | x -> Some x
    | exception Not_found -> None

  (* reorders arguments to improve type inference *)
  let iter list ~f = iter list ~f
end

module Option = struct
  let is_some = function
    | None -> false
    | Some _ -> true

  let iter t ~f =
    match t with
    | None -> ()
    | Some x -> f x

  let map t ~f =
    match t with
    | None -> None
    | Some x -> Some (f x)

  let value t ~default =
    match t with
    | None -> default
    | Some x -> x
end

module Out_channel = struct
  let create ?(binary = true) ?(append = false) ?(fail_if_exists = false) ?(perm = 0o666) file =
    let flags = [Open_wronly; Open_creat] in
    let flags = (if binary then Open_binary else Open_text) :: flags in
    let flags = (if append then Open_append else Open_trunc) :: flags in
    let flags = (if fail_if_exists then Open_excl :: flags else flags) in
    open_out_gen flags perm file

  let with_file ?binary ?append ?fail_if_exists ?perm file ~f =
    let t = create ?binary ?append ?fail_if_exists ?perm file in
    Exn.protectx t ~f ~finally:close_out

  let write_all filename ~data =
    with_file filename ~f:(fun t -> output_string t data)
end

module String = struct
  include String

  let is_empty (t : t) = length t = 0

  let prefix t len = sub t ~pos:0 ~len
  let suffix t len = sub t ~pos:(length t - len) ~len

  let drop_prefix t len = sub t ~pos:len ~len:(length t - len)
  let drop_suffix t len = sub t ~pos:0 ~len:(length t - len)

  let is_prefix t ~prefix =
    let rec is_prefix_from t ~prefix ~pos ~len =
      pos >= len
      || (Char.equal (get t pos) (get prefix pos)
          && is_prefix_from t ~prefix ~pos:(pos + 1) ~len)
    in
    length t >= length prefix && is_prefix_from t ~prefix ~pos:0 ~len:(length prefix)

  let is_suffix t ~suffix =
    let rec is_suffix_up_to t ~suffix ~pos ~suffix_offset =
      pos < 0
      || (Char.equal (get t (suffix_offset + pos)) (get suffix pos)
          && is_suffix_up_to t ~suffix ~pos:(pos - 1) ~suffix_offset)
    in
    length t >= length suffix
    && is_suffix_up_to t
         ~suffix
         ~pos:(length suffix - 1)
         ~suffix_offset:(length t - length suffix)

  let exists t ~f =
    let rec exists_at t ~f ~pos ~len =
      pos < len && (f (get t pos) || exists_at t ~f ~pos:(pos + 1) ~len)
    in
    exists_at t ~f ~pos:0 ~len:(length t)

  let for_all t ~f =
    let rec for_all_at t ~f ~pos ~len =
      pos >= len || (f (get t pos) && for_all_at t ~f ~pos:(pos + 1) ~len)
    in
    for_all_at t ~f ~pos:0 ~len:(length t)

  let index_opt t char =
    match index t char with
    | i -> Some i
    | exception Not_found -> None

  let rindex_opt t char =
    match rindex t char with
    | i -> Some i
    | exception Not_found -> None

  let index_from_opt t char pos =
    match index_from t char pos with
    | i -> Some i
    | exception Not_found -> None

  let rindex_from_opt t char pos =
    match rindex_from t char pos with
    | i -> Some i
    | exception Not_found -> None

  let lsplit2 t ~on =
    match index_opt t on with
    | None -> None
    | Some i -> Some (sub t ~pos:0 ~len:i, sub t ~pos:(i + 1) ~len:(length t - i - 1))

  let capitalize_ascii = Stdlib.String.capitalize_ascii
  let lowercase_ascii = Stdlib.String.lowercase_ascii
  let uncapitalize_ascii = Stdlib.String.uncapitalize_ascii

  let split_on_char t ~sep = Stdlib.String.split_on_char sep t

  include (Poly : Comparisons with type t := string)

  module Map = struct
    include Map.Make (String)

    let find_opt key t =
      match find key t with
      | x -> Some x
      | exception Not_found -> None
  end

  module Set = Set.Make (String)
end

let ( @ ) = List.append

let output oc bytes ~pos ~len = output oc bytes pos len
let output_substring oc string ~pos ~len = output_substring oc string pos len
