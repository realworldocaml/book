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

let rec write_mutual_recurse1 js = (
  Atdgen_codec_runtime.Encode.make (fun (t : mutual_recurse1) ->
    (
    Atdgen_codec_runtime.Encode.obj
      [
          Atdgen_codec_runtime.Encode.field
            (
            write_mutual_recurse2
            )
          ~name:"mutual_recurse2"
          t.mutual_recurse2
      ]
    )
  )
) js
and write_mutual_recurse2 js = (
  Atdgen_codec_runtime.Encode.make (fun (t : mutual_recurse2) ->
    (
    Atdgen_codec_runtime.Encode.obj
      [
          Atdgen_codec_runtime.Encode.field
            (
            write_mutual_recurse1
            )
          ~name:"mutual_recurse1"
          t.mutual_recurse1
      ]
    )
  )
) js
let rec read_mutual_recurse1 js = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      ({
          mutual_recurse2 =
            Atdgen_codec_runtime.Decode.decode
            (
              read_mutual_recurse2
              |> Atdgen_codec_runtime.Decode.field "mutual_recurse2"
            ) json;
      } : mutual_recurse1)
    )
  )
) js
and read_mutual_recurse2 js = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      ({
          mutual_recurse1 =
            Atdgen_codec_runtime.Decode.decode
            (
              read_mutual_recurse1
              |> Atdgen_codec_runtime.Decode.field "mutual_recurse1"
            ) json;
      } : mutual_recurse2)
    )
  )
) js
let rec write__5 js = (
  Atdgen_codec_runtime.Encode.list (
    write_recurse
  )
) js
and write_recurse js = (
  Atdgen_codec_runtime.Encode.make (fun (t : recurse) ->
    (
    Atdgen_codec_runtime.Encode.obj
      [
          Atdgen_codec_runtime.Encode.field
            (
            write__5
            )
          ~name:"recurse_items"
          t.recurse_items
      ]
    )
  )
) js
let rec read__5 js = (
  Atdgen_codec_runtime.Decode.list (
    read_recurse
  )
) js
and read_recurse js = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      ({
          recurse_items =
            Atdgen_codec_runtime.Decode.decode
            (
              read__5
              |> Atdgen_codec_runtime.Decode.field "recurse_items"
            ) json;
      } : recurse)
    )
  )
) js
let write_valid = (
  Atdgen_codec_runtime.Encode.bool
)
let read_valid = (
  Atdgen_codec_runtime.Decode.bool
)
let write_v2 = (
  Atdgen_codec_runtime.Encode.make (fun (x : v2) -> match x with
    | V1_foo x ->
    Atdgen_codec_runtime.Encode.constr1 "V1_foo" (
      Atdgen_codec_runtime.Encode.int
    ) x
    | V2_bar x ->
    Atdgen_codec_runtime.Encode.constr1 "V2_bar" (
      Atdgen_codec_runtime.Encode.bool
    ) x
  )
)
let read_v2 = (
  Atdgen_codec_runtime.Decode.enum
  [
      (
      "V1_foo"
      ,
        `Decode (
        Atdgen_codec_runtime.Decode.int
        |> Atdgen_codec_runtime.Decode.map (fun x -> ((V1_foo x) : v2))
        )
      )
    ;
      (
      "V2_bar"
      ,
        `Decode (
        Atdgen_codec_runtime.Decode.bool
        |> Atdgen_codec_runtime.Decode.map (fun x -> ((V2_bar x) : v2))
        )
      )
  ]
)
let write_v1 = (
  Atdgen_codec_runtime.Encode.make (fun (x : v1) -> match x with
    | V1_foo x ->
    Atdgen_codec_runtime.Encode.constr1 "V1_foo" (
      Atdgen_codec_runtime.Encode.bool
    ) x
    | V2_bar x ->
    Atdgen_codec_runtime.Encode.constr1 "V2_bar" (
      Atdgen_codec_runtime.Encode.int
    ) x
  )
)
let read_v1 = (
  Atdgen_codec_runtime.Decode.enum
  [
      (
      "V1_foo"
      ,
        `Decode (
        Atdgen_codec_runtime.Decode.bool
        |> Atdgen_codec_runtime.Decode.map (fun x -> ((V1_foo x) : v1))
        )
      )
    ;
      (
      "V2_bar"
      ,
        `Decode (
        Atdgen_codec_runtime.Decode.int
        |> Atdgen_codec_runtime.Decode.map (fun x -> ((V2_bar x) : v1))
        )
      )
  ]
)
let write__6 = (
  Atdgen_codec_runtime.Encode.make (fun (t : _) ->
    t |>
    List.map (
      fun (key, value) ->
        Atdgen_codec_runtime.Encode.field
          (
            Atdgen_codec_runtime.Encode.int
          )
          ~name:key
          value
    ) |>
    Atdgen_codec_runtime.Encode.obj
  )
)
let read__6 = (
  Atdgen_codec_runtime.Decode.obj_list (
    Atdgen_codec_runtime.Decode.int
  )
)
let write_using_object = (
  Atdgen_codec_runtime.Encode.make (fun (t : using_object) ->
    (
    Atdgen_codec_runtime.Encode.obj
      [
          Atdgen_codec_runtime.Encode.field
            (
            write__6
            )
          ~name:"f"
          t.f
      ]
    )
  )
)
let read_using_object = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      ({
          f =
            Atdgen_codec_runtime.Decode.decode
            (
              read__6
              |> Atdgen_codec_runtime.Decode.field "f"
            ) json;
      } : using_object)
    )
  )
)
let write_single_tuple = (
  Atdgen_codec_runtime.Encode.make (fun (x : _) -> match x with
    | `Single_tuple x ->
    Atdgen_codec_runtime.Encode.constr1 "Single_tuple" (
      Atdgen_codec_runtime.Encode.tuple1
        (
          Atdgen_codec_runtime.Encode.int
        )
    ) x
  )
)
let read_single_tuple = (
  Atdgen_codec_runtime.Decode.enum
  [
      (
      "Single_tuple"
      ,
        `Decode (
        Atdgen_codec_runtime.Decode.tuple1
          (
            Atdgen_codec_runtime.Decode.int
          )
        |> Atdgen_codec_runtime.Decode.map (fun x -> ((`Single_tuple x) : _))
        )
      )
  ]
)
let write__2 = (
    Atdgen_codec_runtime.Encode.string
  |> Atdgen_codec_runtime.Encode.contramap (function `Id s -> s)
)
let read__2 = (
  (
    Atdgen_codec_runtime.Decode.string
  ) |> (Atdgen_codec_runtime.Decode.map (fun s -> `Id s))
)
let write_id = (
  write__2
)
let read_id = (
  read__2
)
let write__3 = (
  Atdgen_codec_runtime.Encode.make (fun (x : _) -> match x with
    | `Foo x ->
    Atdgen_codec_runtime.Encode.constr1 "Foo" (
      Atdgen_codec_runtime.Encode.tuple2
        (
          Atdgen_codec_runtime.Encode.int
        )
        (
          Atdgen_codec_runtime.Encode.int
        )
    ) x
    | `Bar ->
    Atdgen_codec_runtime.Encode.constr0 "Bar"
    | `Foobar x ->
    Atdgen_codec_runtime.Encode.constr1 "Foobar" (
      Atdgen_codec_runtime.Encode.unit
    ) x
    | `Foo_id x ->
    Atdgen_codec_runtime.Encode.constr1 "Foo_id" (
      write_id
    ) x
  )
)
let read__3 = (
  Atdgen_codec_runtime.Decode.enum
  [
      (
      "Foo"
      ,
        `Decode (
        Atdgen_codec_runtime.Decode.tuple2
          (
            Atdgen_codec_runtime.Decode.int
          )
          (
            Atdgen_codec_runtime.Decode.int
          )
        |> Atdgen_codec_runtime.Decode.map (fun x -> ((`Foo x) : _))
        )
      )
    ;
      (
      "Bar"
      ,
        `Single (`Bar)
      )
    ;
      (
      "Foobar"
      ,
        `Decode (
        Atdgen_codec_runtime.Decode.unit
        |> Atdgen_codec_runtime.Decode.map (fun x -> ((`Foobar x) : _))
        )
      )
    ;
      (
      "Foo_id"
      ,
        `Decode (
        read_id
        |> Atdgen_codec_runtime.Decode.map (fun x -> ((`Foo_id x) : _))
        )
      )
  ]
)
let write__4 = (
  Atdgen_codec_runtime.Encode.list (
    write__3
  )
)
let read__4 = (
  Atdgen_codec_runtime.Decode.list (
    read__3
  )
)
let write_simple_vars = (
  write__4
)
let read_simple_vars = (
  read__4
)
let write_simple_var write__a = (
  Atdgen_codec_runtime.Encode.make (fun (x : _) -> match x with
    | `Foo x ->
    Atdgen_codec_runtime.Encode.constr1 "Foo" (
      Atdgen_codec_runtime.Encode.tuple2
        (
          Atdgen_codec_runtime.Encode.int
        )
        (
          Atdgen_codec_runtime.Encode.int
        )
    ) x
    | `Bar ->
    Atdgen_codec_runtime.Encode.constr0 "Bar"
    | `Foobar x ->
    Atdgen_codec_runtime.Encode.constr1 "Foobar" (
      write__a
    ) x
    | `Foo_id x ->
    Atdgen_codec_runtime.Encode.constr1 "Foo_id" (
      write_id
    ) x
  )
)
let read_simple_var read__a = (
  Atdgen_codec_runtime.Decode.enum
  [
      (
      "Foo"
      ,
        `Decode (
        Atdgen_codec_runtime.Decode.tuple2
          (
            Atdgen_codec_runtime.Decode.int
          )
          (
            Atdgen_codec_runtime.Decode.int
          )
        |> Atdgen_codec_runtime.Decode.map (fun x -> ((`Foo x) : _))
        )
      )
    ;
      (
      "Bar"
      ,
        `Single (`Bar)
      )
    ;
      (
      "Foobar"
      ,
        `Decode (
        read__a
        |> Atdgen_codec_runtime.Decode.map (fun x -> ((`Foobar x) : _))
        )
      )
    ;
      (
      "Foo_id"
      ,
        `Decode (
        read_id
        |> Atdgen_codec_runtime.Decode.map (fun x -> ((`Foo_id x) : _))
        )
      )
  ]
)
let write_same_pair write__a = (
  Atdgen_codec_runtime.Encode.tuple2
    (
      write__a
    )
    (
      write__a
    )
)
let read_same_pair read__a = (
  Atdgen_codec_runtime.Decode.tuple2
    (
      read__a
    )
    (
      read__a
    )
)
let write_record_json_name = (
  Atdgen_codec_runtime.Encode.make (fun (t : record_json_name) ->
    (
    Atdgen_codec_runtime.Encode.obj
      [
          Atdgen_codec_runtime.Encode.field
            (
            Atdgen_codec_runtime.Encode.int
            )
          ~name:"bar"
          t.foo
      ]
    )
  )
)
let read_record_json_name = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      ({
          foo =
            Atdgen_codec_runtime.Decode.decode
            (
              Atdgen_codec_runtime.Decode.int
              |> Atdgen_codec_runtime.Decode.field "bar"
            ) json;
      } : record_json_name)
    )
  )
)
let write_point = (
  Atdgen_codec_runtime.Encode.tuple4
    (
      Atdgen_codec_runtime.Encode.int
    )
    (
      Atdgen_codec_runtime.Encode.int
    )
    (
      Atdgen_codec_runtime.Encode.string
    )
    (
      Atdgen_codec_runtime.Encode.unit
    )
)
let read_point = (
  Atdgen_codec_runtime.Decode.tuple4
    (
      Atdgen_codec_runtime.Decode.int
    )
    (
      Atdgen_codec_runtime.Decode.int
    )
    (
      Atdgen_codec_runtime.Decode.string
    )
    (
      Atdgen_codec_runtime.Decode.unit
    )
)
let write_param_similar write__a = (
  Atdgen_codec_runtime.Encode.make (fun (t : 'a param_similar) ->
    (
    Atdgen_codec_runtime.Encode.obj
      [
          Atdgen_codec_runtime.Encode.field
            (
            write__a
            )
          ~name:"data"
          t.data
        ;
          Atdgen_codec_runtime.Encode.field
            (
            Atdgen_codec_runtime.Encode.int
            )
          ~name:"something"
          t.something
      ]
    )
  )
)
let read_param_similar read__a = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      ({
          data =
            Atdgen_codec_runtime.Decode.decode
            (
              read__a
              |> Atdgen_codec_runtime.Decode.field "data"
            ) json;
          something =
            Atdgen_codec_runtime.Decode.decode
            (
              Atdgen_codec_runtime.Decode.int
              |> Atdgen_codec_runtime.Decode.field "something"
            ) json;
      } : 'a param_similar)
    )
  )
)
let write_param write__a = (
  Atdgen_codec_runtime.Encode.make (fun (t : 'a param) ->
    (
    Atdgen_codec_runtime.Encode.obj
      [
          Atdgen_codec_runtime.Encode.field
            (
            write__a
            )
          ~name:"data"
          t.data
        ;
          Atdgen_codec_runtime.Encode.field
            (
            Atdgen_codec_runtime.Encode.unit
            )
          ~name:"nothing"
          t.nothing
      ]
    )
  )
)
let read_param read__a = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      ({
          data =
            Atdgen_codec_runtime.Decode.decode
            (
              read__a
              |> Atdgen_codec_runtime.Decode.field "data"
            ) json;
          nothing =
            Atdgen_codec_runtime.Decode.decode
            (
              Atdgen_codec_runtime.Decode.unit
              |> Atdgen_codec_runtime.Decode.field "nothing"
            ) json;
      } : 'a param)
    )
  )
)
let write_pair write__a write__b = (
  Atdgen_codec_runtime.Encode.make (fun (t : ('a, 'b) pair) ->
    (
    Atdgen_codec_runtime.Encode.obj
      [
          Atdgen_codec_runtime.Encode.field
            (
            write__a
            )
          ~name:"left"
          t.left
        ;
          Atdgen_codec_runtime.Encode.field
            (
            write__b
            )
          ~name:"right"
          t.right
      ]
    )
  )
)
let read_pair read__a read__b = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      ({
          left =
            Atdgen_codec_runtime.Decode.decode
            (
              read__a
              |> Atdgen_codec_runtime.Decode.field "left"
            ) json;
          right =
            Atdgen_codec_runtime.Decode.decode
            (
              read__b
              |> Atdgen_codec_runtime.Decode.field "right"
            ) json;
      } : ('a, 'b) pair)
    )
  )
)
let write__1 write__a write__b = (
  Atdgen_codec_runtime.Encode.list (
    write_pair write__a write__a
  )
)
let read__1 read__a read__b = (
  Atdgen_codec_runtime.Decode.list (
    read_pair read__a read__a
  )
)
let write_pairs write__a = (
  write__1 write__a write__a
)
let read_pairs read__a = (
  read__1 read__a read__a
)
let write_label = (
  Atdgen_codec_runtime.Encode.string
)
let read_label = (
  Atdgen_codec_runtime.Decode.string
)
let write_labeled = (
  Atdgen_codec_runtime.Encode.make (fun (t : labeled) ->
    (
    Atdgen_codec_runtime.Encode.obj
      [
          Atdgen_codec_runtime.Encode.field
            (
            write_valid
            )
          ~name:"flag"
          t.flag
        ;
          Atdgen_codec_runtime.Encode.field
            (
            write_label
            )
          ~name:"lb"
          t.lb
        ;
          Atdgen_codec_runtime.Encode.field
            (
            Atdgen_codec_runtime.Encode.int
            )
          ~name:"count"
          t.count
      ]
    )
  )
)
let read_labeled = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      ({
          flag =
            Atdgen_codec_runtime.Decode.decode
            (
              read_valid
              |> Atdgen_codec_runtime.Decode.field "flag"
            ) json;
          lb =
            Atdgen_codec_runtime.Decode.decode
            (
              read_label
              |> Atdgen_codec_runtime.Decode.field "lb"
            ) json;
          count =
            Atdgen_codec_runtime.Decode.decode
            (
              Atdgen_codec_runtime.Decode.int
              |> Atdgen_codec_runtime.Decode.field "count"
            ) json;
      } : labeled)
    )
  )
)
let write_from_module_a = (
  A_bs.write_from_module_a
)
let read_from_module_a = (
  A_bs.read_from_module_a
)
let write_b = (
  Atdgen_codec_runtime.Encode.make (fun (t : b) ->
    (
    Atdgen_codec_runtime.Encode.obj
      [
          Atdgen_codec_runtime.Encode.field
            (
            Atdgen_codec_runtime.Encode.int
            )
          ~name:"thing"
          t.thing
      ]
    )
  )
)
let read_b = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      ({
          thing =
            Atdgen_codec_runtime.Decode.decode
            (
              Atdgen_codec_runtime.Decode.int
              |> Atdgen_codec_runtime.Decode.field "thing"
            ) json;
      } : b)
    )
  )
)
let write_a = (
  Atdgen_codec_runtime.Encode.make (fun (t : a) ->
    (
    Atdgen_codec_runtime.Encode.obj
      [
          Atdgen_codec_runtime.Encode.field
            (
            Atdgen_codec_runtime.Encode.string
            )
          ~name:"thing"
          t.thing
        ;
          Atdgen_codec_runtime.Encode.field
            (
            Atdgen_codec_runtime.Encode.bool
            )
          ~name:"other_thing"
          t.other_thing
      ]
    )
  )
)
let read_a = (
  Atdgen_codec_runtime.Decode.make (fun json ->
    (
      ({
          thing =
            Atdgen_codec_runtime.Decode.decode
            (
              Atdgen_codec_runtime.Decode.string
              |> Atdgen_codec_runtime.Decode.field "thing"
            ) json;
          other_thing =
            Atdgen_codec_runtime.Decode.decode
            (
              Atdgen_codec_runtime.Decode.bool
              |> Atdgen_codec_runtime.Decode.field "other_thing"
            ) json;
      } : a)
    )
  )
)
let write_adapted = (
  Atdgen_codec_runtime.Encode.adapter Atdgen_codec_runtime.Json_adapter.Type_field.restore (
    Atdgen_codec_runtime.Encode.make (fun (x : _) -> match x with
      | `A x ->
      Atdgen_codec_runtime.Encode.constr1 "A" (
        write_a
      ) x
      | `B x ->
      Atdgen_codec_runtime.Encode.constr1 "B" (
        write_b
      ) x
    )
  )
)
let read_adapted = (
  Atdgen_codec_runtime.Decode.adapter Atdgen_codec_runtime.Json_adapter.Type_field.normalize (
    Atdgen_codec_runtime.Decode.enum
    [
        (
        "A"
        ,
          `Decode (
          read_a
          |> Atdgen_codec_runtime.Decode.map (fun x -> ((`A x) : _))
          )
        )
      ;
        (
        "B"
        ,
          `Decode (
          read_b
          |> Atdgen_codec_runtime.Decode.map (fun x -> ((`B x) : _))
          )
        )
    ]
  )
)
