(** This is a doc comment with some code blocks in it:

    {[
      # List.map (fun x -> x * x) [(1 + 9); 2; 3]
      - : int list = [100; 4; 9]
    ]}

    {[List.map (fun x -> x * x) [1; 2; 3]]}

    {[
      # List.map (fun x -> x * x) [(1 + 9); 2; 3]
      - : int list = [100; 4; 9]
      # List.map (fun x -> x * x) [1; 2; 3]
      - : int list = [1; 4; 9]
    ]}
*)
val foo : string

(** {[1 + 1 = 3]} *)
val bar : string
