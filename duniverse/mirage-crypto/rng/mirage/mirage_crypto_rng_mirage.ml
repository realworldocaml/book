(*
 * Copyright (c) 2014 Hannes Mehnert
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014-2016 David Kaloper Meršinjak
 * Copyright (c) 2015 Citrix Systems Inc
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)
open Mirage_crypto_rng

let src = Logs.Src.create "mirage-crypto-rng-mirage" ~doc:"Mirage crypto RNG mirage"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (T : Mirage_time.S) (M : Mirage_clock.MCLOCK) = struct
  let rdrand_task g delta =
    let open Lwt.Infix in
    Lwt.async (fun () ->
        let rec one () =
          Entropy.cpu_rng g;
          T.sleep_ns delta >>=
          one
        in
        one ())

  let bootstrap_functions () =
    [ Entropy.bootstrap ; Entropy.bootstrap ;
      Entropy.whirlwind_bootstrap ; Entropy.bootstrap ]

  let running = ref false

  let initialize (type a) ?g ?(sleep = Duration.of_sec 1) (rng : a generator) =
    if !running then
      Lwt.fail_with "entropy collection already running"
    else begin
      (try
         let _ = default_generator () in
         Log.warn (fun m -> m "Mirage_crypto_rng.default_generator has already \
                               been set, check that this call is intentional");
       with
         No_default_generator -> ());
      running := true;
      let seed =
        List.mapi (fun i f -> f i) (bootstrap_functions ()) |> Cstruct.concat
      in
      let rng = create ?g ~seed ~time:M.elapsed_ns rng in
      set_default_generator rng;
      rdrand_task (Some rng) sleep;
      Mirage_runtime.at_enter_iter (Entropy.timer_accumulator (Some rng));
      Lwt.return_unit
    end

  (* For Mirage_random.S compatibility *)
  type nonrec g = g

  let generate ?g l = generate ?g l
end
