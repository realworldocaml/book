open! Base

(* this no longer allocates closures. hurray! *)
module T0 = struct
  type 'a t = 'a list [@@deriving_inline hash]

  
let _ = fun (_ : 'a t) -> ()
let hash_fold_t :
  'a .
    (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state) ->
      Ppx_hash_lib.Std.Hash.state -> 'a t -> Ppx_hash_lib.Std.Hash.state
  = hash_fold_list
let _ = hash_fold_t
[@@@deriving.end]
end

module T1 = struct
  type 'a t = 'a option list [@@deriving_inline hash]

  
let _ = fun (_ : 'a t) -> ()
let hash_fold_t :
  'a .
    (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state) ->
      Ppx_hash_lib.Std.Hash.state -> 'a t -> Ppx_hash_lib.Std.Hash.state
  =
  fun _hash_fold_a ->
    fun hsv ->
      fun arg ->
        hash_fold_list
          (fun hsv -> fun arg -> hash_fold_option _hash_fold_a hsv arg) hsv
          arg
let _ = hash_fold_t
[@@@deriving.end]
end

module T2 = struct
  type 'a t = ('a * 'a) list [@@deriving_inline hash]

  
let _ = fun (_ : 'a t) -> ()
let hash_fold_t :
  'a .
    (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state) ->
      Ppx_hash_lib.Std.Hash.state -> 'a t -> Ppx_hash_lib.Std.Hash.state
  =
  fun _hash_fold_a ->
    fun hsv ->
      fun arg ->
        hash_fold_list
          (fun hsv ->
             fun arg ->
               let (e0, e1) = arg in
               let hsv = _hash_fold_a hsv e0 in
               let hsv = _hash_fold_a hsv e1 in hsv) hsv arg
let _ = hash_fold_t
[@@@deriving.end]
end

module T3 = struct
  type 'a t = Leaf | Node of 'a t list [@@deriving_inline hash]

  
let _ = fun (_ : 'a t) -> ()
let rec hash_fold_t : type a.
  (Ppx_hash_lib.Std.Hash.state -> a -> Ppx_hash_lib.Std.Hash.state) ->
    Ppx_hash_lib.Std.Hash.state -> a t -> Ppx_hash_lib.Std.Hash.state
  =
  fun _hash_fold_a ->
    fun hsv ->
      fun arg ->
        match arg with
        | Leaf -> Ppx_hash_lib.Std.Hash.fold_int hsv 0
        | Node _a0 ->
            let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 1 in
            let hsv = hsv in
            hash_fold_list
              (fun hsv -> fun arg -> hash_fold_t _hash_fold_a hsv arg) hsv
              _a0
let _ = hash_fold_t
[@@@deriving.end]

  let hash_fold_t_no_closure_allocation : type
    a.(Ppx_hash_lib.Std.Hash.state -> a -> Ppx_hash_lib.Std.Hash.state) ->
    Ppx_hash_lib.Std.Hash.state -> a t -> Ppx_hash_lib.Std.Hash.state
    =
    fun _hash_fold_a  ->
      let rec hash_fold_t_of_a =
        fun hsv  ->
        fun arg  ->
          match arg with
          | Leaf  -> Ppx_hash_lib.Std.Hash.fold_int hsv 0
          | Node _a0 ->
            hash_fold_list
              hash_fold_t_of_a
              (Ppx_hash_lib.Std.Hash.fold_int hsv 1) _a0
      in
      hash_fold_t_of_a

end

module T4 = struct
  type 'a t = Leaf | Node of ('a * 'a) t list [@@deriving_inline hash]

  
let _ = fun (_ : 'a t) -> ()
let rec hash_fold_t : type a.
  (Ppx_hash_lib.Std.Hash.state -> a -> Ppx_hash_lib.Std.Hash.state) ->
    Ppx_hash_lib.Std.Hash.state -> a t -> Ppx_hash_lib.Std.Hash.state
  =
  fun _hash_fold_a ->
    fun hsv ->
      fun arg ->
        match arg with
        | Leaf -> Ppx_hash_lib.Std.Hash.fold_int hsv 0
        | Node _a0 ->
            let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 1 in
            let hsv = hsv in
            hash_fold_list
              (fun hsv ->
                 fun arg ->
                   hash_fold_t
                     (fun hsv ->
                        fun arg ->
                          let (e0, e1) = arg in
                          let hsv = _hash_fold_a hsv e0 in
                          let hsv = _hash_fold_a hsv e1 in hsv) hsv arg) hsv
              _a0
let _ = hash_fold_t
[@@@deriving.end]

  let rec hash_fold_t_lazy_closure_allocation : type
    a.(Ppx_hash_lib.Std.Hash.state -> a -> Ppx_hash_lib.Std.Hash.state) ->
    Ppx_hash_lib.Std.Hash.state -> a t -> Ppx_hash_lib.Std.Hash.state
    =
    fun _hash_fold_a  ->
      let hash_fold_t_of_tuple = lazy (
        hash_fold_t_lazy_closure_allocation
          (fun hsv  ->
             fun arg  ->
               let (e0,e1) = arg  in
               _hash_fold_a (_hash_fold_a hsv e0) e1))
      in
      fun hsv  ->
      fun arg  ->
        match arg with
        | Leaf  -> Ppx_hash_lib.Std.Hash.fold_int hsv 0
        | Node _a0 ->
          hash_fold_list
            (Lazy.force hash_fold_t_of_tuple)
            (Ppx_hash_lib.Std.Hash.fold_int hsv 1) _a0

end
