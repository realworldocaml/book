open Core.Std
open Rwo_html

let indexterm_to_idx docs =
  let rec loop item = match item with
    | Data _ -> item
    | Element ("a", attrs, [Data "&nbsp;"]) -> (
      if List.mem attrs ("data-type", "indexterm") then (
        match
          check_attrs attrs ~required:["data-type"; "data-primary"]
        with
        | Error _ ->
          item (* no point in recursing to single Data child *)
        | Ok () ->
          let data = sprintf "%s%s"
            (List.Assoc.find_exn attrs "data-primary")
            (
              try ("/" ^ List.Assoc.find_exn attrs "data-secondary")
              with Not_found -> ""
            )
          in
          let attrs = List.filter attrs ~f:(fun (x,_) ->
            match x with
            | "data-type" | "data-primary" | "data-secondary" -> false
            | _ -> true
          )
          in
          Element("idx", attrs, [Data data])
      )
      else
        item (* no point in recursing to single Data child *)
    )
    | Element (name, attrs, children) ->
      Element(name, attrs, List.map children ~f:loop)
  in
  List.map docs ~f:loop

let idx_to_indexterm t =
  let rec loop item = match item with
    | Data _ -> item
    | Element ("idx", attrs, [Data data]) -> (
      match String.split data ~on:'/' with
      | x::[] ->
        Element (
          "a",
          ["data-type","indexterm"; "data-primary",x]@attrs,
          [Data "&nbsp;"]
        )
      | x::y ->
        Element (
          "a",
          [
            "data-type", "indexterm";
            "data-primary", x;
            "data-secondary", String.concat ~sep:"/" y;
          ]@attrs,
          [Data "&nbsp;"]
        )
      | _ ->
        failwithf
          "<idx> node's child must be slash separated string but got %s"
          data ()
    )
    | Element ("idx", _, _) ->
      failwith "<idx> node should have single Data child"
    | Element (name, attrs, childs) ->
      Element (name, attrs, List.map childs ~f:loop)
  in
  List.map t ~f:loop
