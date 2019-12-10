(* Auto-generated from "test2.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]
open Test

type ('aa, 'bb) poly = ('aa, 'bb) Test.poly

type poly_int2 = (int, int) poly

type test2 = { test0: poly_int2; test1: (int, string option) poly }

type poly_int_string = (int, string) poly

(* Writers for type poly *)

val poly_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!poly}.
      Readers may support more than just this tag. *)

val write_untagged_poly :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'aa -> unit) ->
  (Bi_outbuf.t -> 'aa -> unit) ->
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'bb -> unit) ->
  (Bi_outbuf.t -> 'bb -> unit) ->
  Bi_outbuf.t -> ('aa, 'bb) poly -> unit
  (** Output an untagged biniou value of type {!poly}. *)

val write_poly :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'aa -> unit) ->
  (Bi_outbuf.t -> 'aa -> unit) ->
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'bb -> unit) ->
  (Bi_outbuf.t -> 'bb -> unit) ->
  Bi_outbuf.t -> ('aa, 'bb) poly -> unit
  (** Output a biniou value of type {!poly}. *)

val string_of_poly :
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'aa -> unit) ->
  (Bi_outbuf.t -> 'aa -> unit) ->
  Bi_io.node_tag ->
  (Bi_outbuf.t -> 'bb -> unit) ->
  (Bi_outbuf.t -> 'bb -> unit) ->
  ?len:int -> ('aa, 'bb) poly -> string
  (** Serialize a value of type {!poly} into
      a biniou string. *)

(* Readers for type poly *)

val get_poly_reader :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'aa)) ->
  (Bi_inbuf.t -> 'aa) ->
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'bb)) ->
  (Bi_inbuf.t -> 'bb) ->
  Bi_io.node_tag -> (Bi_inbuf.t -> ('aa, 'bb) poly)
  (** Return a function that reads an untagged
      biniou value of type {!poly}. *)

val read_poly :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'aa)) ->
  (Bi_inbuf.t -> 'aa) ->
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'bb)) ->
  (Bi_inbuf.t -> 'bb) ->
  Bi_inbuf.t -> ('aa, 'bb) poly
  (** Input a tagged biniou value of type {!poly}. *)

val poly_of_string :
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'aa)) ->
  (Bi_inbuf.t -> 'aa) ->
  (Bi_io.node_tag -> (Bi_inbuf.t -> 'bb)) ->
  (Bi_inbuf.t -> 'bb) ->
  ?pos:int -> string -> ('aa, 'bb) poly
  (** Deserialize a biniou value of type {!poly}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type poly_int2 *)

val poly_int2_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!poly_int2}.
      Readers may support more than just this tag. *)

val write_untagged_poly_int2 :
  Bi_outbuf.t -> poly_int2 -> unit
  (** Output an untagged biniou value of type {!poly_int2}. *)

val write_poly_int2 :
  Bi_outbuf.t -> poly_int2 -> unit
  (** Output a biniou value of type {!poly_int2}. *)

val string_of_poly_int2 :
  ?len:int -> poly_int2 -> string
  (** Serialize a value of type {!poly_int2} into
      a biniou string. *)

(* Readers for type poly_int2 *)

val get_poly_int2_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> poly_int2)
  (** Return a function that reads an untagged
      biniou value of type {!poly_int2}. *)

val read_poly_int2 :
  Bi_inbuf.t -> poly_int2
  (** Input a tagged biniou value of type {!poly_int2}. *)

val poly_int2_of_string :
  ?pos:int -> string -> poly_int2
  (** Deserialize a biniou value of type {!poly_int2}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


(* Writers for type test2 *)

val test2_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!test2}.
      Readers may support more than just this tag. *)

val write_untagged_test2 :
  Bi_outbuf.t -> test2 -> unit
  (** Output an untagged biniou value of type {!test2}. *)

val write_test2 :
  Bi_outbuf.t -> test2 -> unit
  (** Output a biniou value of type {!test2}. *)

val string_of_test2 :
  ?len:int -> test2 -> string
  (** Serialize a value of type {!test2} into
      a biniou string. *)

(* Readers for type test2 *)

val get_test2_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> test2)
  (** Return a function that reads an untagged
      biniou value of type {!test2}. *)

val read_test2 :
  Bi_inbuf.t -> test2
  (** Input a tagged biniou value of type {!test2}. *)

val test2_of_string :
  ?pos:int -> string -> test2
  (** Deserialize a biniou value of type {!test2}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)

val create_test2 :
  test0: poly_int2 ->
  test1: (int, string option) poly ->
  unit -> test2
  (** Create a record of type {!test2}. *)


(* Writers for type poly_int_string *)

val poly_int_string_tag : Bi_io.node_tag
  (** Tag used by the writers for type {!poly_int_string}.
      Readers may support more than just this tag. *)

val write_untagged_poly_int_string :
  Bi_outbuf.t -> poly_int_string -> unit
  (** Output an untagged biniou value of type {!poly_int_string}. *)

val write_poly_int_string :
  Bi_outbuf.t -> poly_int_string -> unit
  (** Output a biniou value of type {!poly_int_string}. *)

val string_of_poly_int_string :
  ?len:int -> poly_int_string -> string
  (** Serialize a value of type {!poly_int_string} into
      a biniou string. *)

(* Readers for type poly_int_string *)

val get_poly_int_string_reader :
  Bi_io.node_tag -> (Bi_inbuf.t -> poly_int_string)
  (** Return a function that reads an untagged
      biniou value of type {!poly_int_string}. *)

val read_poly_int_string :
  Bi_inbuf.t -> poly_int_string
  (** Input a tagged biniou value of type {!poly_int_string}. *)

val poly_int_string_of_string :
  ?pos:int -> string -> poly_int_string
  (** Deserialize a biniou value of type {!poly_int_string}.
      @param pos specifies the position where
                 reading starts. Default: 0. *)


