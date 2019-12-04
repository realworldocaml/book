open! Base

type int_hashtbl = int Hashtbl.M(Int).t [@@deriving sexp]

let%test "Hashtbl.merge succeeds with first-class-module interface" =
  let t1 = Hashtbl.create (module Int) in
  let t2 = Hashtbl.create (module Int) in
  let result =
    Hashtbl.merge t1 t2 ~f:(fun ~key:_ -> function
      | `Left x -> x
      | `Right x -> x
      | `Both _ -> assert false)
    |> Hashtbl.to_alist
  in
  List.equal Poly.equal  result []

let%test_module _ = (module Hashtbl_tests.Make(struct
    include Hashtbl

    let create_poly ?size () = Poly.create ?size ()

    let of_alist_poly_exn l = Poly.of_alist_exn l
    let of_alist_poly_or_error l = Poly.of_alist_or_error l
  end))
