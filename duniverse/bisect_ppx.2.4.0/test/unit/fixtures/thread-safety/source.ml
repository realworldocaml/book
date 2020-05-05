let () =
  let repeat n = for i = 1 to n do () done in

  let count = Sys.argv.(1) |> int_of_string in

  let thread_1 = Thread.create repeat count in
  let thread_2 = Thread.create repeat count in

  Thread.join thread_1;
  Thread.join thread_2
