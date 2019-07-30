(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



class type converter =
  object
    method header : string
    method footer : string
    method summary : Report_utils.counts -> string
    method file_header : string -> string
    method file_footer : string -> string
    method file_summary : Report_utils.counts -> string
    method point : int -> int -> string
  end

let output_file verbose in_file conv visited points =
  verbose (Printf.sprintf "Processing file '%s'..." in_file);
  let cmp_content = Hashtbl.find points in_file |> Bisect.Common.read_points' in
  verbose (Printf.sprintf "... file has the following points: %s"
    (String.concat ","
      (List.map (fun pd ->
        Printf.sprintf "[%d %d]\n"
          Bisect.Common.(pd.offset) Bisect.Common.(pd.identifier))
        cmp_content)));
  let len = Array.length visited in
  let stats = Report_utils.make () in
  let points =
    List.map
      (fun p ->
        let nb =
          if Bisect.Common.(p.identifier) < len then
            visited.(Bisect.Common.(p.identifier))
          else
            0 in
        Report_utils.update stats (nb > 0);
        Bisect.Common.(p.offset, nb))
      cmp_content in
  let buffer = Buffer.create 64 in
  Buffer.add_string buffer (conv#file_header in_file);
  Buffer.add_string buffer (conv#file_summary stats);
  List.iter
    (fun (ofs, nb) ->
      Buffer.add_string buffer (conv#point ofs nb))
    points;
  Buffer.add_string buffer (conv#file_footer in_file);
  Some (Buffer.contents buffer, stats)

let output verbose file conv data points =
  let files, stats = Hashtbl.fold
      (fun file visited (files, summary) ->
        match output_file verbose file conv visited points with
        | None -> files, summary
        | Some (text, stats) ->
          ((file, text) :: files, (Report_utils.add summary stats)))
      data
      ([], (Report_utils.make ())) in
  let sorted_files =
    List.sort
      (fun (f1, _) (f2, _) -> compare f1 f2)
      files in
  let write ch =
    output_string ch conv#header;
    output_string ch (conv#summary stats);
    List.iter
      (fun (_, s) ->
        output_string ch s)
      sorted_files;
    output_string ch conv#footer in
  match file with
  | "-" -> write stdout
  | f -> Bisect.Common.try_out_channel false f write
