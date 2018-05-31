open Core_kernel

module Result = struct
  include Result

  module List = struct
    type ('a, 'b) monad = ('a, 'b) t
    type 'a t = 'a list

    let mapi (type error) l ~f =
      let module M = struct
        exception E of error
	let the_fun () =
          let run () =
            List.mapi l ~f:(fun i x ->
              match f i x with
              | Ok o -> o
              | Error e -> raise (E e))
          in
          try Ok (run ())
          with
          | E e -> Error e
      end in
      M.the_fun ()

    let map l ~f =
      mapi l ~f:(fun _ x -> f x)

    let foldi (type error) l ~init ~f =
      let module M = struct
        exception E of error
        let helper () =
          List.foldi l ~init ~f:(fun i accum x ->
            match f i accum x with
            | Ok x -> x
            | Error e -> raise (E e)
          )
      end in
      try Ok (M.helper ())
      with M.E e -> Error e

    let fold l ~init ~f =
      foldi l ~init ~f:(fun _ accum y -> f accum y)

  end
end
