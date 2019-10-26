let x =
  try y with
    | A -> _
    | B -> _

let x = try y with
  | A -> _
  | B -> _

let x =
  try y with
      A -> _
    | B -> _

let x = try y with
    A -> _
  | B -> _

let _ =
  let x =
    try y with
      | A -> _
      | B -> _
  in
  let x = try y with
    | A -> _
    | B -> _
  in
  let x =
    try y with
        A -> _
      | B -> _
  in
  let x = try y with
      A -> _
    | B -> _
