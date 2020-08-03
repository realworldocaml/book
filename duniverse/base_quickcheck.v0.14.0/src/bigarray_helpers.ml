open! Base

module Layout = struct
  type 'a t = 'a Bigarray.layout

  let offset : type a. a t -> int = function
    | Bigarray.Fortran_layout -> 1
    | Bigarray.C_layout -> 0
  ;;
end

module Array1 = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Bigarray.Array1.t

  let iteri t ~f =
    let offset = Layout.offset (Bigarray.Array1.layout t) in
    for i = 0 to Bigarray.Array1.dim t - 1 do
      f (i + offset) t.{i + offset}
    done
  ;;

  let init (type elt) (kind : (elt, _) Bigarray.kind) layout dim ~f =
    let t = Bigarray.Array1.create kind layout dim in
    iteri t ~f:(fun i (_ : elt) -> t.{i} <- f i);
    t
  ;;

  let fold (type elt) (t : (elt, _, _) t) ~init ~f =
    let init = ref init in
    iteri t ~f:(fun i (_ : elt) -> init := f !init t.{i});
    !init
  ;;

  let to_array t =
    let offset = Layout.offset (Bigarray.Array1.layout t) in
    Array.init (Bigarray.Array1.dim t) ~f:(fun i -> t.{i + offset})
  ;;

  let sexp_of_t sexp_of_elt _sexp_of_pack _sexp_of_layout t =
    [%sexp (to_array t : elt array)]
  ;;

  let hash_fold hash_fold_elt state t =
    let state = hash_fold_int state (Bigarray.Array1.dim t) in
    fold t ~init:state ~f:hash_fold_elt
  ;;
end

module Array2 = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Bigarray.Array2.t

  let iteri t ~f =
    let offset = Layout.offset (Bigarray.Array2.layout t) in
    for i = 0 to Bigarray.Array2.dim1 t - 1 do
      for j = 0 to Bigarray.Array2.dim2 t - 1 do
        f (i + offset) (j + offset) t.{i + offset, j + offset}
      done
    done
  ;;

  let init (type elt) (kind : (elt, _) Bigarray.kind) layout dim1 dim2 ~f =
    let t = Bigarray.Array2.create kind layout dim1 dim2 in
    iteri t ~f:(fun i j (_ : elt) -> t.{i, j} <- f i j);
    t
  ;;

  let fold (type elt) (t : (elt, _, _) t) ~init ~f =
    let init = ref init in
    iteri t ~f:(fun (_ : int) (_ : int) elt -> init := f !init elt);
    !init
  ;;

  let to_array t =
    let offset = Layout.offset (Bigarray.Array2.layout t) in
    Array.init (Bigarray.Array2.dim1 t) ~f:(fun i ->
      Array.init (Bigarray.Array2.dim2 t) ~f:(fun j -> t.{i + offset, j + offset}))
  ;;

  let sexp_of_t sexp_of_elt _sexp_of_pack _sexp_of_layout t =
    [%sexp (to_array t : elt array array)]
  ;;

  let hash_fold hash_fold_elt state t =
    let state = hash_fold_int state (Bigarray.Array2.dim1 t) in
    let state = hash_fold_int state (Bigarray.Array2.dim2 t) in
    fold t ~init:state ~f:hash_fold_elt
  ;;
end
