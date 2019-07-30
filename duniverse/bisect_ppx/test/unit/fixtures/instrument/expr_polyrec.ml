let rec f : 'a. 'a -> unit =
  fun _ ->
    f 0;
    f ""

let () =
  let rec f : 'a. 'a -> unit =
    fun _ ->
      f 0;
      f "" in
  f 0
