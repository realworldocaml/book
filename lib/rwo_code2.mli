open Core.Std
open Async.Std

type phrase = {
    input : string;
    output :
      (
        Outcometree.out_phrase * Oloop.Output.merged Oloop.Output.t,
        Oloop.error * string
      ) Result.t;
  }

type part = {
    number : float;
    phrases : phrase list;
  }

type script = {
    filename : string;
    parts : part list;
  }

val eval_script : string -> script Or_error.t Deferred.t
