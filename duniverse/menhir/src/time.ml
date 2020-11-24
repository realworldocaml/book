(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

open Unix
open Printf

let clock =
  ref (times())

let tick msg =
  match Settings.timings with
  | None ->
      ()
  | Some channel ->
      let times1 = !clock in
      let times2 = times() in
      fprintf channel "%s: %.02fs\n%!"
        msg
        (times2.tms_utime -. times1.tms_utime);
      clock := times()

type chrono =
    float ref

let fresh () =
  ref 0.

let chrono (chrono : float ref) (task : unit -> 'a) : 'a =
  match Settings.timings with
  | None ->
      task()
  | Some _channel ->
      let times1 = times() in
      let result = task() in
      let times2 = times() in
      chrono := !chrono +. times2.tms_utime -. times1.tms_utime;
      result

let display (chrono : float ref) msg =
  match Settings.timings with
  | None ->
      ()
  | Some channel ->
      fprintf channel "%s: %.02fs\n"
        msg
        !chrono
