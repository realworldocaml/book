(* About this file:

   It checks the equivalence of the various parsexp parsers, as well some sexplib parsers.
   This check is manual, it is not run in hydra in any way.

   This requires instrumentation, so I build my own compiler like so:
   # ./configure -prefix $PWD/installed -afl-instrument && make world world.opt install
   Without a custom compiler, the stdlib will not be instrumented.

   The way I run it:
   function fuzz-run {
     (
        mkdir -p input
        echo a > input/testcase
        rm -rf output/min-*
        shopt -s nullglob
        export AFL_SKIP_CPUFREQ=
        for prop in {0..14}; do
          export PROP=$prop
          timeout 2m afl-fuzz -i input -o output ./parser_equivalence.exe
          mkdir -p output/min-$prop
          for f in output/crashes/id\:*; do
            afl-tmin -i $f -o output/min-$prop/$(basename $f) -- ./parser_equivalence.exe
          done
        done
     )
   }
   function fuzz-result {
     (
       shopt -s nullglob
       for v in output/min-*; do
         prop=$(echo $(basename $v) | cut -d - -f 2-)
         for f in $(md5sum output/min-$prop/* | sort -u | awk '{ tbl[$1] = $2 } END { for (k in tbl) { print tbl[k] } }' | sort | grep -v '^-$'); do
           echo --- prop:$prop
           PROP=$prop NICE_EXN= ./parser_equivalence.exe < $f
         done
       done
     ) < /dev/null |& less -R
   }

   This is needed because the cli of afl-fuzz is wonky.
   Run fuzz-run to actuall run tests. Run fuzz-result either during the tests or after to
   see the failures so far.
*)

open Base
module P = Parsexp

let compare_exn _ _ = 0
type 'a or_exn = ('a, exn) Result.t
[@@deriving compare, sexp_of]

let parsexp_single str =
  Result.try_with (fun () -> P.Single.parse_string_exn str)

let parsexp_many str =
  Result.try_with (fun () -> P.Many.parse_string_exn str)

let parsexp_eager str ~no_sexp_is_error =
  let r = ref [] in
  let state =
    P.Eager.State.create ~no_sexp_is_error
      (fun _ v -> r := v :: !r)
  in
  Result.try_with (fun () ->
    let stack = P.Eager.Stack.empty in
    let stack = P.Eager.feed_string state str stack in
    P.Eager.feed_eoi state stack;
    List.rev !r)

let parsexp_single_and_positions str =
  Result.try_with (fun () -> P.Single_and_positions.parse_string_exn str)
let parsexp_many_and_positions str =
  Result.try_with (fun () -> P.Many_and_positions.parse_string_exn str)
let parsexp_eager_and_positions str =
  let r = ref [] in
  let state =
    P.Eager_and_positions.State.create ~no_sexp_is_error:false
      (fun _ v -> r := v :: !r)
  in
  Result.try_with (fun () ->
    let stack = P.Eager_and_positions.Stack.empty in
    let stack = P.Eager_and_positions.feed_string state str stack in
    P.Eager_and_positions.feed_eoi state stack;
    List.rev !r)

let parsexp_single_just_positions str =
  Result.try_with (fun () -> P.Single_just_positions.parse_string_exn str)
let parsexp_many_just_positions str =
  Result.try_with (fun () -> P.Many_just_positions.parse_string_exn str)
let parsexp_eager_just_positions str =
  let r = ref [] in
  let state =
    P.Eager_just_positions.State.create ~no_sexp_is_error:false
      (fun _ v -> r := v :: !r)
  in
  Result.try_with (fun () ->
    let stack = P.Eager_just_positions.Stack.empty in
    let stack = P.Eager_just_positions.feed_string state str stack in
    P.Eager_just_positions.feed_eoi state stack;
    List.rev !r)

let parsexp_many_cst str =
  Result.try_with (fun () -> P.Many_cst.parse_string_exn str)
let parsexp_eager_cst str =
  let r = ref [] in
  let state =
    P.Eager_cst.State.create ~no_sexp_is_error:false
      (fun _ v -> r := v :: !r)
  in
  Result.try_with (fun () ->
    let stack = P.Eager_cst.Stack.empty in
    let stack = P.Eager_cst.feed_string state str stack in
    P.Eager_cst.feed_eoi state stack;
    List.rev !r)

let sexplib_single str =
  Result.try_with (fun () -> Sexplib.Sexp.of_string str)

let sexplib_many str =
  Result.try_with (fun () -> Parsexp_test.Import.sexplib_sexps_of_string str)

let single_of_many = function
  | Ok [x] -> Ok x
  | Ok _ -> Error (Failure "not single")
  | Error _ as e -> e

let fst_or_error = function
  | Ok p -> Ok (fst p)
  | Error _ as e -> e

let snd_or_error = function
  | Ok p -> Ok (snd p)
  | Error _ as e -> e

let main ~nice_exn ~property =
  let buf = Buffer.create 100000 in
  AflPersistent.run (fun () ->
    let str =
      ignore (Stdio.In_channel.input_buffer Stdio.In_channel.stdin buf ~len:100000);
      let str = Buffer.contents buf in
      Buffer.reset buf;
      str
    in

    let equal_or_raise (type a) sexp_of_a compare_a name1 name2 v1 v2 =
      if [%compare: a or_exn] v1 v2 <> 0
      then
        if nice_exn
        then
          raise_s [%sexp (str : string)
                      , ((name1 : string), (v1 : a or_exn))
                        , ((name2 : string), (v2 : a or_exn)) ]
        else Caml.raise_notrace Caml.Exit
    in

    match property with
    | 0 ->
      equal_or_raise
        [%sexp_of: Sexp.t] [%compare: Sexp.t]
        "parsexp_single" "parsexp_many"
         (parsexp_single str) (single_of_many (parsexp_many str))

    | 1 ->
      equal_or_raise
        [%sexp_of: Sexp.t list] [%compare: Sexp.t list]
        "parsexp_many" "parsexp_eager_no_sexp_is_error:false"
         (parsexp_many str) (parsexp_eager str ~no_sexp_is_error:false)

    | 2 ->
      equal_or_raise
        [%sexp_of: Sexp.t list] [%compare: Sexp.t list]
        "parsexp_many" "parsexp_eager_no_sexp_is_error:true"
        (match parsexp_many str with Ok [] -> Error (Failure "empty") | x -> x)
        (parsexp_eager str ~no_sexp_is_error:true)

    | 3 ->
      equal_or_raise
        [%sexp_of: Sexp.t] [%compare: Sexp.t]
        "parsexp_single" "parsexp_single_and_positions"
        (parsexp_single str) (fst_or_error (parsexp_single_and_positions str))

    | 4 ->
      equal_or_raise
        [%sexp_of: Sexp.t list] [%compare: Sexp.t list]
        "parsexp_many" "parsexp_many_and_positions"
        (parsexp_many str) (fst_or_error (parsexp_many_and_positions str))

    | 5 ->
      equal_or_raise
        [%sexp_of: Sexp.t list] [%compare: Sexp.t list]
        "parsexp_many" "parsexp_many_and_positions"
        (parsexp_many str)
        (match parsexp_eager_and_positions str with
         | Ok l -> Ok (List.map l ~f:fst)
         | Error _ as e -> e)

    | 6 ->
      equal_or_raise
        [%sexp_of: Sexp.t * P.Positions.t] [%compare: Sexp.t * P.Positions.t]
        "parsexp_single_and_positions" "parsexp_many_and_positions"
        (parsexp_single_and_positions str) (match parsexp_many_and_positions str with
          | Ok ([v], positions) -> Ok (v, positions)
          | Error _ as e -> e
          | Ok (_, _) -> Error (Failure "not many"))

    | 7 ->
      equal_or_raise
        [%sexp_of: P.Positions.t] [%compare: P.Positions.t]
        "parsexp_single_just_positions" "parsexp_single_and_positions"
        (parsexp_single_just_positions str) (snd_or_error (parsexp_single_and_positions str))

    | 8 ->
      equal_or_raise
        [%sexp_of: P.Positions.t] [%compare: P.Positions.t]
        "parsexp_many_just_positions" "parsexp_many_and_positions"
        (parsexp_many_just_positions str) (snd_or_error (parsexp_many_and_positions str))

    | 9 ->
      equal_or_raise
        [%sexp_of: P.Positions.t list] [%compare: P.Positions.t list]
        "parsexp_eager_just_positions" "parsexp_eager_and_positions"
        (parsexp_eager_just_positions str)
        (match parsexp_eager_and_positions str with
         | Ok l -> Ok (List.map l ~f:snd)
         | Error _ as e -> e)

    | 10 ->
      equal_or_raise
        [%sexp_of: P.Positions.pos array] [%compare: P.Positions.pos array]
         "parsexp_many_just_positions" "parsexp_eager_just_positions"
         (match parsexp_many_just_positions str with
         | Ok a -> Ok (P.Positions.to_array a)
         | Error _ as e -> e)
         (match parsexp_eager_and_positions str with
         | Ok l -> Ok (Array.concat_map (Array.of_list l) ~f:(fun a -> P.Positions.to_array (snd a)))
         | Error _ as e -> e)

    | 11 ->
      equal_or_raise
        [%sexp_of: Sexp.t list] [%compare: Sexp.t list]
        "parsexp_many" "parsexp_many_cst"
        (parsexp_many str) (match parsexp_many_cst str with
          | Error _ as e -> e
          | Ok l -> Ok (P.Cst.Forget.t_or_comments l))

    | 12 ->
      equal_or_raise
        [%sexp_of: P.Cst.t_or_comment list] [%compare: P.Cst.t_or_comment list]
        "parsexp_many_cst" "parsexp_eager_cst"
        (parsexp_many_cst str) (parsexp_eager_cst str)

    | 13 ->
      equal_or_raise
        [%sexp_of: Sexp.t] [%compare: Sexp.t]
        "parsexp_single" "sexplib_single"
        (parsexp_single str) (sexplib_single str)

    | 14 ->
      equal_or_raise
        [%sexp_of: Sexp.t list] [%compare: Sexp.t list]
        "parsexp_many" "sexplib_many"
        (parsexp_many str) (sexplib_many str)

    | _ -> raise_s [%sexp "property out of range", (property : int)]
  )

let nice_exn =
  match Caml.Sys.getenv "NICE_EXN" with
  | exception (Not_found_s _ | Caml.Not_found) -> false
  | (_ : string) -> true
    ;;

let property =
  match Caml.Sys.getenv "PROP" with
  | exception (Not_found_s _ | Caml.Not_found) -> assert false
  | s -> Int.of_string s
;;

let () = main ~nice_exn ~property
