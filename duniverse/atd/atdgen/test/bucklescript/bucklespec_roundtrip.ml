
type 'a test =
  { name: string
  ; to_yojson : 'a -> Yojson.Safe.t
  ; of_yojson : Yojson.Safe.t -> 'a
  ; data: 'a
  }

type test' = T : 'a test -> test'

type failure =
  { name: string
  ; actual: Yojson.Safe.t
  ; received: (Yojson.Safe.t, (exn * string)) result
  }

let pp_json fmt json =
  Format.pp_print_string fmt (Yojson.Safe.pretty_to_string ~std:true json)

let pp_received fmt = function
  | Error (e, bt) ->
      Format.fprintf fmt "exn: %s@.backtrace:@.%s"
        (Printexc.to_string e) bt
  | Ok json -> pp_json fmt json

let test_decode ~name ~yojson ~buckle ~data =
  T { name
    ; to_yojson = (fun a -> Yojson.Safe.from_string (yojson a))
    ; of_yojson = Atdgen_codec_runtime.Decode.decode buckle
    ; data
    }

let test_encode ~name ~yojson ~buckle ~data =
  T { name
    ; to_yojson = Atdgen_codec_runtime.Encode.encode buckle
    ; of_yojson = (fun j -> yojson (Yojson.Safe.to_string ~std:false j))
    ; data
    }

let run_test (T t) =
  let json = t.to_yojson t.data in
  let data' =
    try
      Ok (t.of_yojson json)
    with e ->
      Error (e, Printexc.get_backtrace ())
  in
  if Ok t.data = data' then (
    Ok ()
  ) else (
    Error (
      { name = t.name
      ; actual = json
      ; received =
          (match data' with
           | Ok d -> Ok (t.to_yojson d)
           | Error e -> Error e)
      }
    )
  )

let run_tests tests =
  let failures =
    tests
    |> List.fold_left (fun failures t ->
      match run_test t with
      | Ok () -> failures
      | Error fmt -> fmt::failures
    ) [] in
  match failures with
  | [] -> exit 0
  | xs ->
      begin
        xs
        |> List.iter (fun f ->
          Format.eprintf
            "%s: Roundtrip failed.@.Have:%a@.Decoded:%a@."
            f.name
            pp_json f.actual
            pp_received f.received
        );
        exit 1
      end

let () =
  run_tests
    [ test_decode ~name:"decode record"
        ~yojson:Bucklespec_j.string_of_labeled
        ~buckle:Bucklespec_bs.read_labeled
        ~data:{ Bucklespec_t.
                flag = false
              ; lb = "foo bar"
              ; count = 123
              }
    ; test_encode ~name:"encode record"
        ~yojson:Bucklespec_j.labeled_of_string
        ~buckle:Bucklespec_bs.write_labeled
        ~data:{ Bucklespec_t.
                flag = false
              ; lb = "foo bar"
              ; count = 123
              }
    ; test_decode ~name:"decode variant"
        ~yojson:Bucklespec_j.string_of_simple_vars
        ~buckle:Bucklespec_bs.read_simple_vars
        ~data:[ `Foo (123, 456)
              ; `Bar
              ; `Foobar ()
              ; `Foo_id (`Id "testing")
              ]
    ; test_encode ~name:"encode variant"
        ~yojson:Bucklespec_j.simple_vars_of_string
        ~buckle:Bucklespec_bs.write_simple_vars
        ~data:[ `Foo (123, 456)
              ; `Bar
              ; `Foobar ()
              ; `Foo_id (`Id "testing")
              ]
    ; test_encode ~name:"encode 1 tuple"
        ~yojson:Bucklespec_j.single_tuple_of_string
        ~buckle:Bucklespec_bs.write_single_tuple
        ~data:(`Single_tuple (123))
    ; test_decode ~name:"decode adapted variant a"
        ~yojson:Bucklespec_j.string_of_adapted
        ~buckle:Bucklespec_bs.read_adapted
        ~data: Bucklespec_t.(
          `A ({
            thing = "thing";
            other_thing = false;
          }))
    ; test_encode ~name:"encode adapted variant a"
        ~yojson:Bucklespec_j.adapted_of_string
        ~buckle:Bucklespec_bs.write_adapted
        ~data: Bucklespec_t.(
          `A ({
            thing = "thing";
            other_thing = false;
          }))
    ; test_decode ~name:"decode adapted variant b"
        ~yojson:Bucklespec_j.string_of_adapted
        ~buckle:Bucklespec_bs.read_adapted
        ~data: Bucklespec_t.(
          `B ({
            thing = 1;
          }))
    ; test_encode ~name:"encode adapted variant b"
        ~yojson:Bucklespec_j.adapted_of_string
        ~buckle:Bucklespec_bs.write_adapted
        ~data: Bucklespec_t.(
          `B ({
            thing = 1;
          }))
    ]
