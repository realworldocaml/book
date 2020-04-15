(* Auto-generated from "bucklespec.atd" *)
              [@@@ocaml.warning "-27-32-35-39"]

type recurse = Bucklespec_t.recurse = { recurse_items: recurse list }

type mutual_recurse1 = Bucklespec_t.mutual_recurse1 = {
  mutual_recurse2: mutual_recurse2
}

and mutual_recurse2 = Bucklespec_t.mutual_recurse2 = {
  mutual_recurse1: mutual_recurse1
}

type valid = Bucklespec_t.valid

type v2 = Bucklespec_t.v2 =  V1_foo of int | V2_bar of bool 

type v1 = Bucklespec_t.v1 =  V1_foo of bool | V2_bar of int 

type using_object = Bucklespec_t.using_object = { f: (string * int) list }

type single_tuple = Bucklespec_t.single_tuple

type id = Bucklespec_t.id

type 'a simple_var = 'a Bucklespec_t.simple_var

type simple_vars = Bucklespec_t.simple_vars

type 'a same_pair = 'a Bucklespec_t.same_pair

type record_json_name = Bucklespec_t.record_json_name = { foo: int }

type point = Bucklespec_t.point

type 'a param_similar = 'a Bucklespec_t.param_similar = {
  data: 'a;
  something: int
}

type 'a param = 'a Bucklespec_t.param = { data: 'a; nothing: unit }

type ('a, 'b) pair = ('a, 'b) Bucklespec_t.pair = { left: 'a; right: 'b }

type 'a pairs = 'a Bucklespec_t.pairs

type label = Bucklespec_t.label

type labeled = Bucklespec_t.labeled = { flag: valid; lb: label; count: int }

type from_module_a = A_t.from_module_a

type b = Bucklespec_t.b = { thing: int }

type a = Bucklespec_t.a = { thing: string; other_thing: bool }

type adapted = Bucklespec_t.adapted

val read_recurse :  recurse Atdgen_codec_runtime.Decode.t

val write_recurse :  recurse Atdgen_codec_runtime.Encode.t

val read_mutual_recurse1 :  mutual_recurse1 Atdgen_codec_runtime.Decode.t

val write_mutual_recurse1 :  mutual_recurse1 Atdgen_codec_runtime.Encode.t

val read_mutual_recurse2 :  mutual_recurse2 Atdgen_codec_runtime.Decode.t

val write_mutual_recurse2 :  mutual_recurse2 Atdgen_codec_runtime.Encode.t

val read_valid :  valid Atdgen_codec_runtime.Decode.t

val write_valid :  valid Atdgen_codec_runtime.Encode.t

val read_v2 :  v2 Atdgen_codec_runtime.Decode.t

val write_v2 :  v2 Atdgen_codec_runtime.Encode.t

val read_v1 :  v1 Atdgen_codec_runtime.Decode.t

val write_v1 :  v1 Atdgen_codec_runtime.Encode.t

val read_using_object :  using_object Atdgen_codec_runtime.Decode.t

val write_using_object :  using_object Atdgen_codec_runtime.Encode.t

val read_single_tuple :  single_tuple Atdgen_codec_runtime.Decode.t

val write_single_tuple :  single_tuple Atdgen_codec_runtime.Encode.t

val read_id :  id Atdgen_codec_runtime.Decode.t

val write_id :  id Atdgen_codec_runtime.Encode.t

val read_simple_var : 'a Atdgen_codec_runtime.Decode.t -> 'a simple_var Atdgen_codec_runtime.Decode.t

val write_simple_var : 'a Atdgen_codec_runtime.Encode.t -> 'a simple_var Atdgen_codec_runtime.Encode.t

val read_simple_vars :  simple_vars Atdgen_codec_runtime.Decode.t

val write_simple_vars :  simple_vars Atdgen_codec_runtime.Encode.t

val read_same_pair : 'a Atdgen_codec_runtime.Decode.t -> 'a same_pair Atdgen_codec_runtime.Decode.t

val write_same_pair : 'a Atdgen_codec_runtime.Encode.t -> 'a same_pair Atdgen_codec_runtime.Encode.t

val read_record_json_name :  record_json_name Atdgen_codec_runtime.Decode.t

val write_record_json_name :  record_json_name Atdgen_codec_runtime.Encode.t

val read_point :  point Atdgen_codec_runtime.Decode.t

val write_point :  point Atdgen_codec_runtime.Encode.t

val read_param_similar : 'a Atdgen_codec_runtime.Decode.t -> 'a param_similar Atdgen_codec_runtime.Decode.t

val write_param_similar : 'a Atdgen_codec_runtime.Encode.t -> 'a param_similar Atdgen_codec_runtime.Encode.t

val read_param : 'a Atdgen_codec_runtime.Decode.t -> 'a param Atdgen_codec_runtime.Decode.t

val write_param : 'a Atdgen_codec_runtime.Encode.t -> 'a param Atdgen_codec_runtime.Encode.t

val read_pair : 'a Atdgen_codec_runtime.Decode.t -> 'b Atdgen_codec_runtime.Decode.t -> ('a, 'b) pair Atdgen_codec_runtime.Decode.t

val write_pair : 'a Atdgen_codec_runtime.Encode.t -> 'b Atdgen_codec_runtime.Encode.t -> ('a, 'b) pair Atdgen_codec_runtime.Encode.t

val read_pairs : 'a Atdgen_codec_runtime.Decode.t -> 'a pairs Atdgen_codec_runtime.Decode.t

val write_pairs : 'a Atdgen_codec_runtime.Encode.t -> 'a pairs Atdgen_codec_runtime.Encode.t

val read_label :  label Atdgen_codec_runtime.Decode.t

val write_label :  label Atdgen_codec_runtime.Encode.t

val read_labeled :  labeled Atdgen_codec_runtime.Decode.t

val write_labeled :  labeled Atdgen_codec_runtime.Encode.t

val read_from_module_a :  from_module_a Atdgen_codec_runtime.Decode.t

val write_from_module_a :  from_module_a Atdgen_codec_runtime.Encode.t

val read_b :  b Atdgen_codec_runtime.Decode.t

val write_b :  b Atdgen_codec_runtime.Encode.t

val read_a :  a Atdgen_codec_runtime.Decode.t

val write_a :  a Atdgen_codec_runtime.Encode.t

val read_adapted :  adapted Atdgen_codec_runtime.Decode.t

val write_adapted :  adapted Atdgen_codec_runtime.Encode.t

