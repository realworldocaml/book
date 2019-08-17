let%test_module _ =
  (module struct
    let x = ref 0
    let init = lazy (x := 42)

    module Inline_test_config = struct
      include Inline_test_config
      let pre_test_hook () = Lazy.force init
    end

    let%test _ = !x = 42
  end)
