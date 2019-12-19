(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

let list_map_cps : ('a -> 'b cps) -> 'a list -> 'b list cps =
    fun f l throw k ->

  let rec loop accumulator = function
    | [] -> k (List.rev accumulator)
    | x::l -> f x throw (fun x' -> loop (x'::accumulator) l)
  in
  loop [] l

module Parsing =
struct
  type context_entry =
    {f        : string -> string option;
     previous : context_entry}

  type context = context_entry ref

  let parse qualified_name =
    try
      let colon_index = String.index qualified_name ':' in
      let prefix = String.sub qualified_name 0 colon_index in
      let suffix =
        String.sub qualified_name
          (colon_index + 1)
          (String.length qualified_name - colon_index - 1)
      in
      prefix, suffix

    with Not_found -> ("", qualified_name)

  let init top_level =
    let f = function
      | "xml" -> Some xml_ns
      | "xmlns" -> Some xmlns_ns
      | s -> top_level s
    in
    let rec entry = {f; previous = entry} in
    ref entry

  let expand_element report context raw_element_name throw k =
    let ns, name = parse raw_element_name in
    match !context.f ns with
    | Some uri -> k (uri, name)
    | None ->
      match ns with
      | "" -> k ("", name)
      | prefix ->
        report () (`Bad_namespace prefix) throw (fun () -> k (prefix, name))

  let push report context raw_element_name raw_attributes throw k =
    let parsed_attributes =
      raw_attributes |> List.map (fun (name, value) -> parse name, value) in

    let f =
      parsed_attributes |> List.fold_left (fun f -> function
        | ("xmlns", prefix), uri ->
          (fun p -> if p = prefix then Some uri else f p)
        | ("", "xmlns"), uri ->
          (fun p -> if p = "" then Some uri else f p)
        | _ -> f)
        !context.f
    in

    let entry = {f; previous = !context} in
    context := entry;

    expand_element report context raw_element_name throw
      (fun expanded_element_name ->
    list_map_cps begin fun (name, value) _ k ->
      match name with
      | "", "xmlns" -> k ((xmlns_ns, "xmlns"), value)
      | "", name -> k (("", name), value)
      | ns, name ->
        match f ns with
        | Some uri -> k ((uri, name), value)
        | None ->
          report () (`Bad_namespace ns) throw (fun () -> k ((ns, name), value))
    end parsed_attributes throw (fun expanded_attributes ->
    k (expanded_element_name, expanded_attributes)))

  let pop ({contents = {previous}} as context) =
    context := previous
end

module StringMap = Map.Make (String)

module Writing =
struct
  type context_entry =
    {namespace_to_prefix : string list StringMap.t;
     prefix_to_namespace : string StringMap.t;
     previous            : context_entry}

  type context = context_entry ref * (string -> string option)

  let init top_level =
    let namespace_to_prefix =
      StringMap.empty
      |> StringMap.add "" [""]
      |> StringMap.add xml_ns ["xml"]
      |> StringMap.add xmlns_ns ["xmlns"]
    in

    let prefix_to_namespace =
      StringMap.empty
      |> StringMap.add "" ""
      |> StringMap.add "xml" xml_ns
      |> StringMap.add "xmlns" xmlns_ns
    in

    let rec entry =
      {namespace_to_prefix; prefix_to_namespace; previous = entry} in

    ref entry, top_level

  let lookup report allow_default context namespace throw k =
    let candidate_prefixes =
      try StringMap.find namespace !(fst context).namespace_to_prefix
      with Not_found -> []
    in

    let prefix =
      try
        Some (candidate_prefixes |> List.find (fun prefix ->
          (allow_default || prefix <> "") &&
           begin
            try StringMap.find prefix !(fst context).prefix_to_namespace =
              namespace
            with Not_found -> false
           end))
      with Not_found -> None
    in

    let prefix =
      match prefix with
      | Some _ -> prefix
      | None ->
        match snd context namespace with
        | None -> None
        | Some prefix ->
          if not allow_default && prefix = "" ||
              StringMap.mem prefix !(fst context).prefix_to_namespace then
            None
          else Some prefix
    in

    match prefix with
    | None -> report () (`Bad_namespace namespace) throw (fun () -> k "")
    | Some prefix -> k prefix

  let format prefix name =
    match prefix with
    | "" -> name
    | prefix -> prefix ^ ":" ^ name

  let unexpand_element report context (namespace, name) throw k =
    lookup report true context namespace throw (fun prefix ->
    k (format prefix name))

  let unexpand_attribute report context ((namespace, name), value) throw k =
    match namespace with
    | "" -> k (name, value)
    | uri ->
      if uri = xmlns_ns && name = "xmlns" then k ("xmlns", value)
      else
        lookup report false context namespace throw (fun prefix ->
          k (format prefix name, value))

  let extend k v map =
    let vs =
      try StringMap.find k map
      with Not_found -> []
    in
    StringMap.add k (v::vs) map

  let push report context element_name attributes throw k =
    let namespace_to_prefix, prefix_to_namespace =
      attributes |> List.fold_left (fun (ns_to_prefix, prefix_to_ns) -> function
        | (ns, "xmlns"), uri when ns = xmlns_ns ->
          extend uri "" ns_to_prefix,
          StringMap.add "" uri prefix_to_ns
        | (ns, prefix), uri when ns = xmlns_ns ->
          extend uri prefix ns_to_prefix,
          StringMap.add prefix uri prefix_to_ns
        | _ -> ns_to_prefix, prefix_to_ns)
        (!(fst context).namespace_to_prefix, !(fst context).prefix_to_namespace)
    in

    let entry =
      {namespace_to_prefix; prefix_to_namespace; previous = !(fst context)} in
    (fst context) := entry;

    unexpand_element report context element_name throw (fun element_name ->
    list_map_cps (unexpand_attribute report context) attributes throw
      (fun attributes ->
    k (element_name, attributes)))

  let pop ({contents = {previous}}, _ as context) =
    (fst context) := previous
end
