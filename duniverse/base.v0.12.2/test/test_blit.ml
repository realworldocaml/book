open! Import
open! Blit

(* This unit test checks that when [blit] calls [unsafe_blit], the slices are valid.
   It also checks that [blit] doesn't call [unsafe_blit] when there is a range error. *)
let%test_module _ =
  (module struct

    let blit_was_called = ref false

    let slices_are_valid = ref (Ok ())

    module B =
      Make
        (struct
          type t = bool array
          let create ~len = Array.create false ~len
          let length = Array.length
          let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
            blit_was_called := true;
            slices_are_valid :=
              Or_error.try_with (fun () ->
                assert (len >= 0);
                assert (src_pos >= 0);
                assert (src_pos + len <= Array.length src);
                assert (dst_pos >= 0);
                assert (dst_pos + len <= Array.length dst));
            Array.blit ~src ~src_pos ~dst ~dst_pos ~len;
          ;;
        end)
    ;;

    let%test_module "Bool" =
      (module Test_blit.Test
           (struct
             type t = bool
             let equal = Bool.equal
             let of_bool = Fn.id
           end)
           (struct
             type t = bool array [@@deriving sexp_of]
             let create ~len = Array.create false ~len
             let length = Array.length
             let get = Array.get
             let set = Array.set
           end)
           (B))
    ;;

    let%test_unit _ =
      let opts = [ None; Some (-1); Some 0; Some 1; Some 2 ] in
      List.iter [ 0; 1; 2 ] ~f:(fun src ->
        List.iter [ 0; 1; 2 ] ~f:(fun dst ->
          List.iter opts ~f:(fun src_pos ->
            List.iter opts ~f:(fun src_len ->
              List.iter opts ~f:(fun dst_pos ->
                try begin
                  let check f =
                    blit_was_called := false;
                    slices_are_valid := Ok ();
                    match Or_error.try_with f with
                    | Error _ -> assert (not !blit_was_called);
                    | Ok () -> ok_exn !slices_are_valid
                  in
                  check (fun () ->
                    B.blito
                      ~src:(Array.create ~len:src false) ?src_pos ?src_len
                      ~dst:(Array.create ~len:dst false) ?dst_pos
                      ());
                  check (fun () ->
                    ignore (B.subo (Array.create ~len:src false) ?pos:src_pos ?len:src_len
                            : bool array));
                end
                with exn ->
                  raise_s [%message
                    "failure"
                      (exn : exn)
                      (src : int) (src_pos : int option) (src_len : int option)
                      (dst : int) (dst_pos : int option)])))))
    ;;
  end)
