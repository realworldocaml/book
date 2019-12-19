(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open Core

type t = {
  regression_name  : string option;
  responder        : Variable.t;
  predictors       : Variable.t list;
  bootstrap_trials : int;
  r_square         : bool;
}

let create ~responder ~predictors
      ?(bootstrap_trials=0) ?(r_square=false) ?regression_name () = {
  responder; predictors; bootstrap_trials; r_square; regression_name;
}

let vs_runs responder () = create ~responder ~predictors:[`Runs] ()
let vs_runs_overhead responder () = create ~responder ~predictors:[`Runs; `One] ()

let nanos_vs_runs = vs_runs `Nanos ()

let cycles_vs_runs = vs_runs `Cycles ()

let allocations_vs_runs = [
  vs_runs_overhead `Minor_allocated ();
  vs_runs_overhead `Major_allocated ();
  vs_runs_overhead `Promoted ();
]

let gc_vs_runs = [
  vs_runs `Minor_collections ();
  vs_runs `Major_collections ();
  vs_runs `Compactions ();
]

let nanos  ~predictors = create ~responder:`Nanos ~predictors ()
let cycles  ~predictors = create ~responder:`Cycles ~predictors ()

(* This includes a lot of things. *)
let default = [ nanos_vs_runs ] @ allocations_vs_runs @ gc_vs_runs

let default_bootstrap_trials = 3000
let default_reduced_bootstrap_trials = 300

let with_error_estimation ?(bootstrap_trials=default_bootstrap_trials) t =
  { t with bootstrap_trials; r_square=true }

let reduce_bootstrap t ~bootstrap_trials =
  if t.bootstrap_trials > bootstrap_trials
  then { t with bootstrap_trials; }
  else t

let make_key t =
  let init = ((Variable.to_int t.responder) lsl Variable.max_int) in
  (List.fold ~init t.predictors ~f:(fun acc pred ->
     (1 lsl (Variable.to_int pred)) + acc))


let parse ?regression_name str =
  let str, bootstrap_trials, r_square =
    match String.chop_prefix str ~prefix:"+" with
    | None -> str, 0, false
    | Some str -> str, default_bootstrap_trials, true
  in
  match String.lsplit2 str ~on:':' with
  | None -> failwith "Regression string should have format <responder:predictors>."
  | Some (resp, preds) ->
    let preds = String.split preds ~on:',' in
    let responder = Variable.of_short_string resp in
    let predictors = List.map preds ~f:Variable.of_short_string in
    create ~responder ~predictors ~bootstrap_trials ~r_square ?regression_name ()

