let exit_flag = ref true

let exit_on_error () = exit_flag := true

let resume_on_error () = exit_flag := false

exception Error of Position.t list * string

let print_error positions msg =
  Printf.sprintf "%s%s\n"
    (String.concat "\n"
       (List.map (fun p -> Position.string_of_pos p ^ ": ") positions))
    msg

let error_alert positions msg =
  if !exit_flag then (
    output_string stderr (print_error positions msg);
    exit 1 )
  else raise (Error (positions, msg))

let global_error kind msg =
  error_alert [] (Printf.sprintf "Global Error (%s)\n  %s" kind msg)

let errorN kind poss msg =
  error_alert poss (Printf.sprintf "Error (%s)\n  %s" kind msg)

let error kind pos = errorN kind [ pos ]

let error2 kind pos1 pos2 = errorN kind [ pos1; pos2 ]
