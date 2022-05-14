open! Core

(** This module renders text graphs to visualize data on the terminal. A typical text
    graph looks like a bar graph as shown below:

    {v
      label1     3786 |----+----1----+----2---- ... ----+----7----+----8----+----9----+----|
      label2     3653 |----+----1----+----2---- ... ----+----7----+----8----+----9----+-
      label3     1055 |----+----1----+----2----+--
      label4      308 |----+---
      label5       70 |-
      label6      404 |----+----1
      label7       71 |-
      label8       32 |
     (each '-' is approximately 37.860 units.)
    v}

*)

(** Renders a text graph given labels and values.  The list should be non-empty and the
    specified values must be positive numbers. Setting narrow enables narrower graphs. *)
val render : ?narrow:bool -> (string * float) list -> string
