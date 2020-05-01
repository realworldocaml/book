(* Json adapters *)

module Identity = struct
  let normalize x = x
  let restore x = x
end
