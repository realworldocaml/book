type point = {
  configuration : string;
  parameter : int;
  time : float;
  time_err : float * float;
  cycles : float;
  cycles_err : float * float;
}

let split_on_colon s =
  let i = String.index s ':' in
  String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)

let early_float s = Scanf.sscanf s "%f" (fun x -> x)

let point_of_row row =
  let configuration, parameter = split_on_colon row.(0) in
  let parameter = int_of_string parameter in
  {
    configuration;
    parameter;
    time=early_float row.(2);
    time_err=early_float row.(3), early_float row.(4);
    cycles=early_float row.(6);
    cycles_err=early_float row.(7), early_float row.(8);
  }

let split_on_spaces s =
  let rec acc lst s =
    let t = String.trim s in
    match String.index t ' ' with
    | si ->
      let f = String.sub t 0 si in
      let r = String.sub t si (String.length t - si) in
      acc (f::lst) r
    | exception Not_found ->
      List.rev (t::lst)
  in
  acc [] s

let print_pretty_point
    ({ configuration; parameter; time; time_err; cycles; cycles_err }) =
  Printf.printf "Configuration: %s\nParameter: %d\n" configuration parameter;
  Printf.printf "Time: %f %f %+f\nCycles: %f %f %+f\n\n"
    time (fst time_err) (snd time_err) cycles (fst cycles_err) (snd cycles_err)

;;

let benchmark_names = [
  "interpreted_local";
  "interpreted_shared";
  "staged_functor";
  "staged_no_functor";
  "traditional";
  "cowboy";
] in
let columns = 9 in

if Array.length Sys.argv < 2
then failwith "must provide benchmark summary file"
else

  let data = ref [] in

  let path = Sys.argv.(1) in
  let ic = open_in path in

  try
    while true do
      let line = input_line ic in
      let prefix = String.sub line 0 6 in
      if prefix = " -----"
      then
        while true do
          let line = input_line ic in
          data := line :: !data
        done
    done
  with End_of_file ->
    close_in ic;
    let table = List.rev_map split_on_spaces !data in
    let table = List.filter (fun row -> List.length row = columns) table in
    let points = List.map (fun row -> point_of_row (Array.of_list row)) table in
    List.iter (fun c ->
      let points = List.filter (function
        | { configuration } when configuration = c -> true
        | _ -> false
      ) points in
      let data_file = c ^ ".txt" in
      let oc = open_out data_file in
      List.iter (fun { parameter; time; cycles } ->
        Printf.fprintf oc "%d\t%f\t%f\n" parameter time cycles
      ) points;
      close_out oc
    ) benchmark_names
