(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)

let file_json verbose indent in_file resolver visited points =

  verbose (Printf.sprintf "Processing file '%s'..." in_file);
  match resolver in_file with
  | None ->
    verbose "... file not found";
    None
  | Some resolved_in_file ->
    let cmp_content =
      Hashtbl.find points in_file |> Bisect.Common.read_points' in
    verbose (Printf.sprintf "... file has %d points" (List.length cmp_content));
    let len = Array.length visited in
    let pts = (List.map
                 (fun p ->
                   let nb =
                     if Bisect.Common.(p.identifier) < len then
                       visited.(Bisect.Common.(p.identifier))
                     else
                       0 in
                   (Bisect.Common.(p.offset), nb))
                 cmp_content) in
    let digest = Digest.to_hex (Digest.file resolved_in_file) in
    let in_channel = open_in resolved_in_file in
    let line_counts =
      try
        let rec read number acc pts =
          try
            let _ = input_line in_channel in
            let end_ofs = pos_in in_channel in
            let before, after =
              Report_utils.split (fun (o, _) -> o < end_ofs) pts in
            let visited_lowest =
              List.fold_left
                (fun v (_, nb) ->
                  match v with
                  | None -> Some nb
                  | Some nb' -> if nb < nb' then Some nb else Some nb')
                None
                before
            in
            read (number + 1) (visited_lowest::acc) after
          with End_of_file -> List.rev acc
        in
        read 1 [] pts
      with e ->
        close_in_noerr in_channel;
        raise e;
    in
    close_in_noerr in_channel;
    let scounts = List.map (function
      | None -> "null"
      | Some nb -> Printf.sprintf "%d" nb) line_counts in
    let coverage = String.concat "," scounts in
    let indent_strings indent l =
      let i = String.make indent ' ' in
      List.map (fun s -> i ^ s) l in
    Some (
      [
        "{";
        Printf.sprintf "    \"name\": \"%s\"," in_file;
        Printf.sprintf "    \"source_digest\": \"%s\"," digest;
        Printf.sprintf "    \"coverage\": [%s]" coverage;
        "}";
      ]
      |> indent_strings indent
      |> String.concat "\n"
    )

let output verbose file service_name service_job_id repo_token resolver data points =
  Report_utils.mkdirs (Filename.dirname file);
  let file_jsons =
    Hashtbl.fold
      (fun in_file visited acc ->
        let maybe_json = file_json verbose 8 in_file resolver visited points in
        match maybe_json with
        | None -> acc
        | Some s -> s :: acc)
      data
      []
  in
  let repo_params =
    [ "service_name", (String.trim service_name) ;
      "service_job_id", (String.trim service_job_id) ;
      "repo_token", (String.trim repo_token) ; ]
    |> List.filter (fun (_, v) -> (String.length v) > 0)
    |> List.map (fun (n, v) ->
      Printf.sprintf "    \"%s\": \"%s\"," n v)
    |> String.concat "\n"
  in
  let write ch =
    Report_utils.output_strings
      [ "{" ;
        repo_params ;
        "    \"source_files\": [" ;
        (String.concat ",\n" file_jsons) ;
        "    ]" ;
        "}" ;
      ]
      []
      ch
  in
  match file with
  | "-" -> write stdout
  | f -> Bisect.Common.try_out_channel false f write
