(* We open (or import) the Alcotest module. This makes all of the functions of
   the Alcotest module available. *)
open Alcotest

(* The Alcotest tests have the signature [unit -> unit], which means they take a
   unit [()] (the ML equivalent of void, i.e. nothing) as the only argument, and
   return a unit. *)
let test_greet () =
  (* We call the [greet] function defined in our [hello] library with the
     argument "Tom" and store the result in a [greeting] variable. *)
  let greeting = Hello.greet "Tom" in
  let expected = "Hello Tom!" in
  (* The [check] and [string] come from Alcotest.

     This line will make sure that [expected] and [greeting] have the same
     value, or fail. *)
  check string "same string" expected greeting

(* This defined a new test suite.

   A test suite in Alcotest is a list of (name, flag, test_function)*)
let suite = [ ("can greet", `Quick, test_greet) ]

(* This line runs the test runner.

   We define the name of the test runner, here "hello", and the different test
   suites to run. Here, there's only one test suite called "Hello". *)
let () = Alcotest.run "hello" [ ("Hello", suite) ]
