#ext json
type mytype = string list
#endext
let data = [ "Hello"; "world" ]
let () = print_endline (J.string_of_mytype data)
