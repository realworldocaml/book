open! Core_kernel
open! Import
open! Binable

let%test_unit _ =
  let module M = struct
    type t = int [@@deriving bin_io]
  end
  in
  let m = (module M : S with type t = int) in
  List.iter
    [ Int.min_value; Int.min_value / 2; -1; 0; 1; Int.max_value / 2; Int.max_value ]
    ~f:(fun i ->
      let check name of_x to_x =
        let i' = of_x m (to_x m i) in
        if i <> i'
        then
          Error.failwiths
            ~here:[%here]
            (Printf.sprintf "Binable.{of,to}_%s failure" name)
            (i, `Round_tripped_to i')
            [%sexp_of: int * [ `Round_tripped_to of int ]]
      in
      check "string" of_string to_string;
      check "bigstring" of_bigstring to_bigstring)
;;

let%test_unit "Of_sexpable" =
  let module M = struct
    type t = int

    include Of_sexpable_without_uuid [@alert "-legacy"] (struct
        type t = int [@@deriving sexp]
      end)
  end
  in
  let m = (module M : S with type t = M.t) in
  List.iter [ Int.min_value; -1; 0; 1; Int.max_value ] ~f:(fun int ->
    [%test_result: int] ~expect:int (of_string m (to_string m int)))
;;
