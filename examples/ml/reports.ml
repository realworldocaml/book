open Core.Std

module Rstats : sig
  type t
  val create : unit -> t
  val add_sample : t -> float -> unit
  val mean : t -> float
  val stdev : t -> float
end = struct
    type t = { mutable sum    : float;
               mutable sum_sq : float;
               mutable samples: float;
             }
    let create () =
      { sum = 0.; sum_sq = 0.; samples = 0. }

    let add_sample t x =
      t.sum     <- t.sum     +. x;
      t.sum_sq  <- t.sum_sq  +. x *. x;
      t.samples <- t.samples +. 1.

    let mean t = t.sum /. t.samples
    let stdev t = (t.sum_sq -. t.sum) /. t.samples
end

module Single_responder_sq_error = struct
    type t = { total    : Rstats.t;
               residual : Rstats.t;
             }

    let create () =
      { total    = Rstats.create ();
        residual = Rstats.create (); }

    let update t ~predicted ~actual =
      Rstats.add_sample t.total actual;
      Rstats.add_sample t.residual (actual -. predicted)

    let summarize t =
      let stats kind rstat =
        sprintf "%s -- mean: %F, stdev: %F\n" kind (Rstats.mean rstat) (Rstats.stdev rstat)
      in
      stats "total" t.total
      ^ stats "residual" t.residual
  end
