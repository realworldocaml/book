open! Core
open! Import
module Time_source = Async_kernel.Time_source

let run_cycles_until_no_jobs_remain = Kernel_scheduler.run_cycles_until_no_jobs_remain

let%bench_module "Clock.every" =
  (module struct
    let scheduler = Kernel_scheduler.t ()
    let time_source = scheduler.time_source |> Time_source.of_synchronous

    let%bench "~continue-on-error:false" =
      let iv = Ivar.create () in
      let n = ref 0 in
      Time_source.run_repeatedly
        time_source
        ~stop:(Ivar.read iv)
        ~continue_on_error:false
        ~f:(fun () ->
          if !n >= 1_000 then Ivar.fill iv () else incr n;
          return ())
        ~continue:Time_source.Continue.immediately;
      run_cycles_until_no_jobs_remain ()
    ;;

    let%bench "~continue_on_error:true" =
      let iv = Ivar.create () in
      let n = ref 0 in
      Time_source.run_repeatedly
        time_source
        ~stop:(Ivar.read iv)
        ~continue_on_error:true
        ~f:(fun () ->
          if !n >= 1_000 then Ivar.fill iv () else incr n;
          return ())
        ~continue:Time_source.Continue.immediately;
      run_cycles_until_no_jobs_remain ()
    ;;
  end)
;;
