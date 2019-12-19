open! Core_kernel
open Poly
open! Import

module Test (Base : sig
    type t

    val of_string : string -> t
  end)
    (Substring : Substring.S with type base = Base.t) : sig end = struct
  open Substring

  let%test_module "get" =
    (module struct
      let hello = Base.of_string "hello"

      let%test _ =
        let lo = create ~pos:3 ~len:2 hello in
        Char.equal (get lo 1) 'o'
      ;;

      let%test _ = Exn.does_raise (fun () -> get (create ~pos:1 ~len:3 hello) 3)
      let%test _ = Exn.does_raise (fun () -> get (create hello) (-1))
    end)
  ;;

  let%test_module "sub" =
    (module struct
      let base = Base.of_string "012345"
      let t = create ~pos:1 ~len:4 base

      (* 1234 *)

      let%test_unit _ = ignore (sub ~pos:0 ~len:4 t : t)
      let%test _ = Exn.does_raise (fun () -> sub ~pos:0 ~len:5 t)
      let%test _ = Exn.does_raise (fun () -> sub ~pos:1 ~len:4 t)

      let%test _ =
        let t2 = sub t ~pos:1 ~len:3 in
        String.init (length t2) ~f:(get t2) = "234"
      ;;

      let%test _ =
        let t2 = sub t ~pos:3 in
        String.init (length t2) ~f:(get t2) = "4"
      ;;

      let%test_unit "empty substring" = ignore (sub ~pos:2 ~len:0 t : t)
      let%test "invalid empty substring" = Exn.does_raise (fun () -> sub ~pos:5 ~len:0 t)
    end)
  ;;

  let%test_module _ =
    (module struct
      let ell = create ~pos:1 ~len:3 (Base.of_string "hello")

      let%test _ = to_array ell = [| 'e'; 'l'; 'l' |]
      let%test _ = to_list ell = [ 'e'; 'l'; 'l' ]
      let%test _ = fold ell ~init:[] ~f:(fun acc x -> x :: acc) = [ 'l'; 'l'; 'e' ]

      let%test _ =
        let stuff = ref [] in
        iter ell ~f:(fun c -> stuff := c :: !stuff);
        !stuff = [ 'l'; 'l'; 'e' ]
      ;;
    end)
  ;;

  let%test_module _ =
    (module struct
      let bcdefghi = create ~pos:1 ~len:8 (Base.of_string "abcdefghijklmno ")

      let%test _ = find bcdefghi ~f:Char.is_lowercase = Some 'b'
      let%test _ = find bcdefghi ~f:Char.is_whitespace = None
      let%test _ = exists bcdefghi ~f:(Char.equal 'h')
      let%test _ = not (exists bcdefghi ~f:(Char.equal 'z'))
      let%test _ = for_all bcdefghi ~f:Char.is_alpha
      let%test _ = not (for_all bcdefghi ~f:(Char.equal 'h'))
      let%test _ = not (mem bcdefghi 'a')
      let%test _ = mem bcdefghi 'b'
      let%test _ = mem bcdefghi 'i'
      let%test _ = not (mem bcdefghi 'j')
      let%test _ = count bcdefghi ~f:(String.mem "aeiou") = 2

      let%test _ =
        sum (module Int) bcdefghi ~f:(fun c -> Char.to_int c - Char.to_int 'a')
        = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8
      ;;

      let%test _ = min_elt bcdefghi ~compare:Char.compare = Some 'b'
      let%test _ = max_elt bcdefghi ~compare:Char.compare = Some 'i'
    end)
  ;;
end

include Test (Bytes) (Substring)

include Test
    (struct
      include Bigstring

      let of_string s = of_string s
    end)
    (Bigsubstring)
