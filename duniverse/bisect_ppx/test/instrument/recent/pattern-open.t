Or-pattern under local open.

  $ bash ../test.sh <<'EOF'
  > module M = struct exception E end
  > let _ =
  >   match () with
  >   | () -> ()
  >   | M.(exception (E | Exit)) -> ()
  > EOF
  module M = struct
    exception E
  end
  
  let _ =
    match () with
    | M.((exception ((E | Exit) as ___bisect_matched_value___))) ->
        (match[@ocaml.warning "-4-8-9-11-26-27-28-33"]
           ___bisect_matched_value___
         with
        | M.(E) ->
            ___bisect_visit___ 1;
            ()
        | M.(Exit) ->
            ___bisect_visit___ 2;
            ()
        | _ -> ());
        ()
    | () ->
        ___bisect_visit___ 0;
        ()
