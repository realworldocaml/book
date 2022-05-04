(***********************************************************************)
(*                                                                     *)
(*                         Async_graphics                              *)
(*                                                                     *)
(*                          Leo White                                  *)
(*                                                                     *)
(*  Copyright 2013 Leo White.  All rights reserved.  This file is      *)
(*  distributed under the terms of the GNU Library General Public      *)
(*  License, with the special exception on linking described in        *)
(*  file LICENSE.                                                      *)
(*                                                                     *)
(***********************************************************************)

open Core
open Async
open Thread_safe_pipe.Written_or_closed
open Thread_safe_pipe.If_closed

include Graphics

(* Setup the event pipe *)

let event_reader = ref None

let close_event_reader () =
  match !event_reader with
    None -> ()
  | Some rd ->
      event_reader := None;
      Pipe.close_read rd

let rec event_loop wr = 
  try
    let status = 
      wait_next_event [Button_down; Button_up; Key_pressed; Mouse_motion] 
    in
      match Thread_safe_pipe.write wr status ~if_closed:Return with
      | Written -> event_loop wr
      | Closed -> ()
  with Graphic_failure _ -> close_event_reader ()
    

let create_event_reader () = 
  match !event_reader with
    Some rd -> Deferred.return rd
  | None ->
      let (rd_d, fill) = Thread_safe.deferred () in
        ignore (Thread.create 
                  (fun () -> 
                   let (rd, wr) = Thread_safe_pipe.create () in
                     event_reader := Some rd;
                     fill rd;
                     event_loop wr) ());
        rd_d

(* Event handlers *)

type handler = 
  { f: status -> unit;
    stop: unit Deferred.t; }

let click_handlers = ref []

let mousedown_handlers = ref []

let mouseup_handlers = ref []

let mousemove_handlers = ref []

let keypress_handlers = ref []

let run_handlers handlers_ref status = 
  let rec loop handlers acc =
    match handlers with
      {f; stop} as handler :: rest ->
        if Deferred.is_determined stop then
          loop rest acc
        else begin
          f status;
          loop rest (handler :: acc)
          end
    | [] -> acc
  in
    handlers_ref := loop (List.rev !handlers_ref) []

let previous_status =  
  ref { mouse_x = -1; 
        mouse_y = -1; 
        button = false; 
        keypressed = false; 
        key = Char.min_value }

let click_status = ref None

let handle_event status =
  let prev = !previous_status in
    if (not prev.button) && status.button then begin
       run_handlers mousedown_handlers status;
      click_status := Some status
    end;
    if prev.button && (not status.button) then begin
      run_handlers mouseup_handlers status;
      match !click_status with
         Some {mouse_x; mouse_y; _} ->  
           if status.mouse_x = mouse_x && status.mouse_y = mouse_y then
             run_handlers click_handlers status
           else
             click_status := None
       | None -> ()
    end;
    if (prev.mouse_x <> status.mouse_x) || (prev.mouse_y <> status.mouse_y) then begin
      run_handlers mousemove_handlers status;
      click_status := None
    end;
    if status.keypressed then
      run_handlers keypress_handlers status;
    previous_status := status

let event_handling_started = ref false

let start_event_handling () =
  if not !event_handling_started then begin
    event_handling_started := true;
    let rec handle_events () =
      create_event_reader () >>> fun event_reader ->
        let rec loop () = 
          (Pipe.read event_reader) >>> function 
             `Eof -> handle_events ()
           | `Ok status ->
               handle_event status;
               loop ()
        in
          loop ()
    in
      handle_events ()
  end

let on_click ?(start = Deferred.unit) ?(stop = Deferred.never ()) f = 
  start >>> (fun () -> 
    start_event_handling ();
    click_handlers := {f; stop} :: !click_handlers)

let on_mousedown ?(start = Deferred.unit) ?(stop = Deferred.never ()) f = 
  start >>> (fun () -> 
    start_event_handling ();
    mousedown_handlers := {f; stop} :: !mousedown_handlers)

let on_mouseup ?(start = Deferred.unit) ?(stop = Deferred.never ()) f = 
  start >>> (fun () -> 
    start_event_handling ();
    mouseup_handlers := {f; stop} :: !mouseup_handlers)

let on_mousemove ?(start = Deferred.unit) ?(stop = Deferred.never ()) f = 
  start >>> (fun () -> 
    start_event_handling ();
    mousemove_handlers := {f; stop} :: !mousemove_handlers)

let on_keypress ?(start = Deferred.unit) ?(stop = Deferred.never ()) f = 
  start >>> (fun () -> 
    start_event_handling ();
    keypress_handlers := {f; stop} :: !keypress_handlers)
