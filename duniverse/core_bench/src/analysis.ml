open Core
open Linear_algebra_wrapper

module M = Measurement
module AC = Analysis_config



let analyze_one meas analysis_config =
  let resp = analysis_config.AC.responder in
  let preds = Array.of_list analysis_config.AC.predictors in
  let open Or_error.Monad_infix in
  ols meas ~resp ~preds
  >>= fun coeffs ->
  let r_square =
    if analysis_config.AC.r_square
    then Some (r_square meas ~resp ~preds ~coeffs)
    else None
  in
  let trials = analysis_config.AC.bootstrap_trials in
  (if trials > 0 then begin
     bootstrap ~trials meas ~resp ~preds >>| fun bs ->
     (Array.mapi preds ~f:(fun i predictor ->
       Analysis_result.Coefficient.create
         ~predictor
         ~estimate:coeffs.(i)
         ~ci95:bs.(i)
         ()))
   end else begin
     Ok (Array.mapi preds ~f:(fun i predictor ->
        Analysis_result.Coefficient.create
         ~predictor
         ~estimate:coeffs.(i)
         ()))
   end)
  >>| fun coefficients ->
  Analysis_result.Regression.create
    ~responder:resp
    ?r_square
    ~coefficients
    ~regression_name:analysis_config.Analysis_config.regression_name
    ()


let analyze meas analysis_configs =
  Or_error.of_exn_result
    (Result.try_with
       (fun () ->
          Analysis_result.create
            ~name:(M.name meas)
            ~test_name:(M.test_name meas)
            ~file_name:(M.file_name meas)
            ~module_name:(M.module_name meas)
            ~sample_count:(M.sample_count meas)
            ~largest_run:(M.largest_run meas)
            ~regressions:(Array.of_list_map analysis_configs
                            ~f:(fun analysis_config ->
                              Or_error.ok_exn
                                (Or_error.tag (analyze_one meas analysis_config)
                                   ~tag:(M.name meas))))))
