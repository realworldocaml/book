let f x =
  (fun x -> x [ (fun () -> 3) ;
                (fun () -> 4) ])

let f x = (fun x -> x [ (fun () -> 3) ;
                        (fun () -> 4) ])

let f x =
  x [ (fun () -> 3) ;
      (fun () -> 4) ]

let f x =
  [ (fun () -> 3) ;
    (fun () -> 4) ]

let f x =
  (fun x -> x [ (fun () ->
       3) ;
       (fun () -> 4) ])

let f x = (fun x -> x [ (fun () ->
    3) ;
    (fun () -> 4) ])

let f x =
  x [ (fun () ->
      3) ;
      (fun () -> 4) ]

let f x =
  [ (fun () ->
        3) ;
    (fun () -> 4) ]
