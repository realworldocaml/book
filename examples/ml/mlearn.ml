open Core.Std

(* A model suitable for training *)
module type Model = sig
  type t
  type predictor
  type responder
  type config

  (** Creates an (empty) model from a model config *)
  val create : config -> t

  (** Updates a model with a predictor/responder pair to be used for training
      the model *)
  val train : t -> predictor * responder -> unit
end

(** A distillation of a [Model.t] optimized for making predictions. *)
module type Predictive_model = sig
  type t
  type model
  type predictor
  type responder

  (** Construct a predictive model based on the current training data.  This may
      be a compute-intensive operation.  *)
  val create : model -> t

    (** Construct a predicted responder *)
  val predict : t -> predictor -> responder
end


(** A mechanism for reporting on the efficiency of a given predictor *)
module type Report = sig
  type t
  type predictor
  type responder

  (** Creates an empty report *)
  val create : unit -> t

  (** Updates a report with a pair of a prediction and an actual outcome.  *)
  val update
    :  t
    -> predictor:predictor
    -> predicted:responder
    -> actual:responder
    -> unit

  (** Returns a human-readable summary of the data generated thus far *)
  val summarize : t -> string
end

module Make_harness
  (Predictor : T)
  (Responder : T)
  (Config    : T)
  (Model : Model
   with type config := Config.t
    and type predictor := Predictor.t
    and type responder := Responder.t
  )
  (Predictive_model : Predictive_model
   with type model     := Model.t
    and type predictor := Predictor.t
    and type responder := Responder.t)
  (Report : Report
   with type predictor := Predictor.t
   with type responder := Responder.t
  ) :
sig

  val train_and_evaluate
    :  Config.t
    -> training      : (Predictor.t * Responder.t) array
    -> out_of_sample : (Predictor.t * Responder.t) array
    -> Report.t

end = struct
  let train_and_evaluate config ~training ~out_of_sample =
    (* Create the initial (empty) model *)
    let model = Model.create config in
    (* Train the model *)
    Array.iter training ~f:(Model.train model);
    (* Create a predictive model, and then generate a report based on the
       predictions of that model on the out-of-sample data *)
    let pmodel = Predictive_model.create model in
    let report = Report.create () in
    Array.iter out_of_sample ~f:(fun (p,r) ->
      let predicted = Predictive_model.predict pmodel p in
      Report.update report ~predictor:p ~predicted ~actual:r);
    report
end
