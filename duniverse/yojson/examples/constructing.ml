(*
  dune exec examples/constructing.exe <<EOF
{
  "id": "398eb027",
  "name": "John Doe",
  "pages": [
    {
      "id": 1,
      "title": "The Art of Flipping Coins",
    }
  ]
}
EOF
*)

open Yojson.Basic.Util

let json_output =
  `Assoc
    [
      ("id", `String "398eb027");
      ("name", `String "John Doe");
      ( "pages",
        `Assoc
          [ ("id", `Int 1); ("title", `String "The Art of Flipping Coins") ] );
    ]

let main () =
  let oc = stdout in
  Yojson.Basic.pretty_to_channel oc json_output;
  output_string oc "\n"

let () = main ()
