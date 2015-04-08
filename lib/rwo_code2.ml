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

let eval_script filename =
  let f_ok oloop : script Deferred.t =
    let add_phrase_to_part {number;phrases} (phrase:string)
        : part Deferred.t
      =
      Oloop.eval oloop phrase >>| fun output ->
      {number; phrases = {input=phrase; output}::phrases}
    in
    let add_part_to_script {filename;parts} {Oloop.Script.number; content}
        : script Deferred.t
      =
      let phrases : string list = Oloop.Script.phrases_of_string content in
      Deferred.List.fold phrases ~init:{number; phrases=[]}
        ~f:add_phrase_to_part
      >>= fun {number;phrases} -> return {number; phrases = List.rev phrases}
      >>| fun part -> {filename; parts = part::parts}
    in
    Oloop.Script.of_file filename >>= fun (parts) ->
    let parts = (ok_exn parts : Oloop.Script.t :> Oloop.Script.part list) in
    Deferred.List.fold parts ~init:{filename; parts=[]}
      ~f:add_part_to_script
    >>| fun {filename;parts} -> {filename; parts = List.rev parts}
  in
  let f oloop = f_ok oloop >>| fun x -> Ok x in
  Oloop.with_toploop Oloop.Output.merged ~f
