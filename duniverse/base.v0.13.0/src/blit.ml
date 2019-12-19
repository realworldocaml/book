open! Import
include Blit_intf

module type Sequence_gen = sig
  type 'a t

  val length : _ t -> int
end

module Make_gen
    (Src : Sequence_gen) (Dst : sig
                            include Sequence_gen

                            val create_like : len:int -> 'a Src.t -> 'a t
                            val unsafe_blit : ('a Src.t, 'a t) blit
                          end) =
struct
  let unsafe_blit = Dst.unsafe_blit

  let blit ~src ~src_pos ~dst ~dst_pos ~len =
    Ordered_collection_common.check_pos_len_exn
      ~pos:src_pos
      ~len
      ~total_length:(Src.length src);
    Ordered_collection_common.check_pos_len_exn
      ~pos:dst_pos
      ~len
      ~total_length:(Dst.length dst);
    if len > 0 then unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len
  ;;

  let blito
        ~src
        ?(src_pos = 0)
        ?(src_len = Src.length src - src_pos)
        ~dst
        ?(dst_pos = 0)
        ()
    =
    blit ~src ~src_pos ~len:src_len ~dst ~dst_pos
  ;;

  (* [sub] and [subo] ensure that every position of the created sequence is populated by
     an element of the source array.  Thus every element of [dst] below is well
     defined. *)
  let sub src ~pos ~len =
    Ordered_collection_common.check_pos_len_exn ~pos ~len ~total_length:(Src.length src);
    let dst = Dst.create_like ~len src in
    if len > 0 then unsafe_blit ~src ~src_pos:pos ~dst ~dst_pos:0 ~len;
    dst
  ;;

  let subo ?(pos = 0) ?len src =
    sub
      src
      ~pos
      ~len:
        (match len with
         | Some i -> i
         | None -> Src.length src - pos)
  ;;
end

module Make1 (Sequence : sig
    include Sequence_gen

    val create_like : len:int -> 'a t -> 'a t
    val unsafe_blit : ('a t, 'a t) blit
  end) =
  Make_gen (Sequence) (Sequence)

module Make1_generic (Sequence : Sequence1) = Make_gen (Sequence) (Sequence)

module Make (Sequence : sig
    include Sequence

    val create : len:int -> t
    val unsafe_blit : (t, t) blit
  end) =
struct
  module Sequence = struct
    type 'a t = Sequence.t

    open Sequence

    let create_like ~len _ = create ~len
    let length = length
    let unsafe_blit = unsafe_blit
  end

  include Make_gen (Sequence) (Sequence)
end

module Make_distinct
    (Src : Sequence) (Dst : sig
                        include Sequence

                        val create : len:int -> t
                        val unsafe_blit : (Src.t, t) blit
                      end) =
  Make_gen
    (struct
      type 'a t = Src.t

      open Src

      let length = length
    end)
    (struct
      type 'a t = Dst.t

      open Dst

      let length = length
      let create_like ~len _ = create ~len
      let unsafe_blit = unsafe_blit
    end)

module Make_to_string (T : sig
    type t
  end)
    (To_bytes : S_distinct with type src := T.t with type dst := bytes) =
struct
  open To_bytes

  let sub src ~pos ~len =
    Bytes0.unsafe_to_string ~no_mutation_while_string_reachable:(sub src ~pos ~len)
  ;;

  let subo ?pos ?len src =
    Bytes0.unsafe_to_string ~no_mutation_while_string_reachable:(subo ?pos ?len src)
  ;;
end
