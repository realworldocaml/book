open Core.Std

(** A trainable model *)
module type MODEL = sig
  type t
  type config
  type predictor
  type responder

  (** Creates an (empty) model from a model config *)
  val create  : config -> t

  (** Updates a model with a predictor/responder pair to be used for training the model *)
  val train  : t -> predictor * responder -> unit

  (** A Predictive_model is a distillation of the model optimized for fast construction of
      predictions.  *)
  module Predictive_model : sig
    type model = t
    type t

    (** Construct a predictive model based on the current training data.  *)
    val create : model -> t

    (** Construct a predicted responder *)
    val predict : t -> predictor -> responder
  end
end

(** A mechanism for reporting on the efficiency of a given predictor *)
module type REPORT = sig
  type t
  type responder

  (** Creates an empty report *)
  val create : unit -> t

  (** Updates a report with a pair of a prediction and an actual outcome.  *)
  val update : t -> predicted : responder -> actual : responder -> unit

  (** Returns a human-readable summary of the data generated thus far *)
  val summarize : t -> string
end

module Make_harness
  (Model : MODEL)
  (Report : REPORT with type responder = Model.responder) :
sig

  val train_and_evaluate
    :  Model.config
    -> training      : (Model.predictor * Model.responder) array
    -> out_of_sample : (Model.predictor * Model.responder) array
    -> Report.t

end = struct
  let train_and_evaluate config ~training ~out_of_sample =
    (* Create the initial (empty) model *)
    let model = Model.create config in
    (* Train the model *)
    Array.iter training ~f:(Model.train model);
    (* Create a predictive model, and then generate a report based on the predictions of
       that model on the out-of-sample data *)
    let pmodel = Model.Predictive_model.create model in
    let report = Report.create () in
    Array.iter out_of_sample ~f:(fun (p,r) ->
      let predicted = Model.Predictive_model.predict pmodel p in
      Report.update report ~predicted ~actual:r);
    report
  end

(* Version of MODEL that is suitable for parallelization *)
module type PAR_MODEL = sig
  type t with bin_io
  val merge : t list -> t
  include MODEL with type t := t
end

