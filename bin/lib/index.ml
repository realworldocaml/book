open Core
open Html

let nbsp = `Data (List.hd_exn (Soup.(texts (parse "&nbsp;"))))

let indexterm_to_idx docs =
  let rec loop item = match item with
    | `Data _ -> item
    | `Element {name="a"; attrs; childs=[x]} when Base.Poly.(x = nbsp) -> (
      if List.mem ~equal:Util.string_pair_equal attrs ("data-type", "indexterm")
      then (
        match
          check_attrs attrs ~required:["data-type"; "data-primary"]
        with
        | Error _ ->
          item (* no point in recursing to single Data child *)
        | Ok () ->
          let data = sprintf "%s%s"
            (List.Assoc.find_exn ~equal:String.equal attrs "data-primary")
            (match List.Assoc.find ~equal:String.equal attrs "data-secondary" with
             | None -> ""
             | Some s -> "/" ^ s)
          in
          let attrs = List.filter attrs ~f:(fun (x,_) ->
            match x with
            | "data-type" | "data-primary" | "data-secondary" -> false
            | _ -> true
          )
          in
          let attrs = ("class", "idx") :: attrs in
          `Element {name="span"; attrs; childs=[`Data data]}
      )
      else
        item (* no point in recursing to single Data child *)
    )
    | `Element {name; attrs; childs} ->
      `Element {name; attrs; childs = List.map childs ~f:loop}
  in
  List.map docs ~f:loop

let idx_to_indexterm t =
  let rec loop item = match item with
    | `Data _ -> item
    | `Element {name="span"; attrs; childs=[`Data data]}
      when Base.Poly.(List.Assoc.find ~equal:String.equal attrs "class" = Some "idx") ->
      let attrs = List.filter attrs ~f:(fun (x, _) -> String.(x <> "class")) in
      (match String.split data ~on:'/' with
      | x::[] ->
        `Element {
          name = "a";
          attrs = ["data-type","indexterm"; "data-primary",x]@attrs;
          childs = [nbsp];
        }
      | x::y ->
        `Element {
          name = "a";
          attrs = [
            "data-type", "indexterm";
            "data-primary", x;
            "data-secondary", String.concat ~sep:"/" y;
          ]@attrs;
          childs = [nbsp];
        }
      | _ ->
        failwithf
          "<idx> node's child must be slash separated string but got %s"
          data ()
    )
    | `Element {name; attrs; childs} ->
      `Element {name; attrs; childs = List.map childs ~f:loop}
  in
  List.map t ~f:loop
