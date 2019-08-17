## v0.11

Use ppxlib instead of (now deprecated) ppx\_core, ppx\_driver and
ppx\_type\_conv.

## 113.43.00

- use the new context-free API

- Code created by ppx_assert used to contain the expansion of
  `[%here]`, which contains the number of preceding chars in the file,
  even though that value doesn't matter (only the line and column
  number matters).

  The consequence of this is noise when diffing .pp files, and since it's easy to avoid,
  let's do it.

  The change in the generated code looks like this:

                     ~expect:(Some next)
                     (next_clock_shift (find_exn "Europe/London") ~after)
                 let expect_prev before prev =
                   (fun ?(here= ``)  ->
                      fun ?message  ->
                        fun ?equal  ->
                          fun ~expect  ->
                            fun got  ->
    -|                        let pos =
    -|                          {
    -|                            Lexing.pos_fname = "lib/core/src/zone.ml";
    -|                            pos_lnum = 673;
    -|                            pos_cnum = 24276;
    -|                            pos_bol = 24257
    -|                          } in
    +|                        let pos = "lib/core/src/zone.ml:673:19" in
                              let sexpifier =
                                sexp_of_option
                                  (function
                                   | (v0,v1) ->
                                       let v0 = Time_as_float.sexp_of_t v0
                                       and v1 = Span.sexp_of_t v1 in
                                       Sexplib.Sexp.List `v0; v1`) in
                              let comparator

  If you have many `%test_..`, the executable will contain the
  filename repeatedly whereas before perhaps the compiler would share
  it. We don't think it matters, especially given that not allocating
  the record probably saves some space and inline tests can be dropped
  at compile time.

## 113.24.00

- Update to follow evolution of `Ppx_core`.
