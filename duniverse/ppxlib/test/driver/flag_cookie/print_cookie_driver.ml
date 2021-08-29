open Ppxlib

let value_x = ref ""

let f = function
  | Some value_of_x ->
      value_x := Printf.sprintf "Value of cookie x: %i" value_of_x
  | None -> value_x := "Cookie x isn't set."

let () = Ppxlib.Driver.Cookies.(add_simple_handler ~f "x" Ast_pattern.(eint __))

let print_cookie_x =
  object
    inherit Ast_traverse.map as super

    method! structure str =
      let new_str =
        List.fold_left
          (fun acc str_item ->
            match str_item with
            | [%stri [@@@print_cookie_x]] ->
                let _ = print_endline !value_x in
                acc
            | _ -> str_item :: acc)
          [] str
      in
      super#structure (List.rev new_str)
  end

let () =
  Driver.register_transformation ~impl:print_cookie_x#structure "test_cookies"

let () = Ppxlib.Driver.standalone ()
