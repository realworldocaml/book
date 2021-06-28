(* If we make a curried binding of afterAll, BuckleScript will compile it like
   this:

     afterAll((function (param) { // <-- notice "param"
       // ... snip ...
     }));

   this will cause timeout in Jest because Jest will wait until you manually
   invoke param.done() within afterAll. *)
external afterAll : ((unit -> unit) [@bs]) -> unit = "afterAll"
  [@@bs.val]

let () =
  afterAll
    ((fun ()  ->
      Runtime.write_coverage_data ();
      Runtime.reset_coverage_data ())
    [@bs])
