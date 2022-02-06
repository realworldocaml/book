(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(** Compute solutions to the Matrix Chain Ordering Problem (MCOP).
    The problem is to find the association of a chain of matrix multiplications
    that minimize the number of multiplications. *)

(** A problem instance is given as an array of integers, representing the
    dimensions of matrices.
    The product of matrices [M1 : M[i,j] x M2[j,k]] is specified by the array
    [[|i; j; k|]].  Thus, the multiplication of n matrices is represented by an
    array of length (n+1). *)
type problem = int array

(** The solution is given as a binary tree of matrices.  Only the shape of the
    binary tree matters, the ['a] parameter is provided as a convenience for
    user to store custom information.  Initially, the 'a is just an integer
    that represents the index of the matrix to multiply (the values grows from
    [0] to [n-1]). *)
type 'a solution =
  | Matrix of 'a
  | Product of 'a solution * 'a solution

(** This exception is raised when an empty problem is submitted, as it has no
    solution. *)
exception Empty

(** Map a solution to a user-chosen representation, to make it more convenient
    to interpret *)
val map_solution : ('a -> 'b) -> 'a solution -> 'b solution

(** Left-leaning product tree.
    This solution is naive solution, the dimensions are not even looked at so
    nothing is optimized.  For benchmarking purposes. *)
val left_solution : problem -> int solution

(** Right-leaning product tree.
    This solution is naive solution, the dimensions are not even looked at so
    nothing is optimized.  For benchmarking purposes. *)
val right_solution : problem -> int solution

(** An implementation of the algorithm described in
    "An O(n) algorithm for determining a near-optimal computation order of
    matrix chain products" by Francis Y. Chin.
    It gives an approximate solution in O(n) time that does less than 25% more
    operations that the optimal one (and much less in practice). *)
val approx_solution : problem -> int solution

(** An implementation of the dynamic programming algorithm.
    It gives an optimal solution in O(n^3).  A formal description can be found
    in "Introduction to Algorithms" by Cormen et al. *)
val dynamic_solution : problem -> int solution

(** TODO An algorithm to compute an optimal solution in O(n log n) is described
    in "Computation of Matrix Chain Products" by Hu and Shing:
    - "Part I", doi 10.1.1.695.2923
    - "Part II", doi 10.1.1.695.4875
    This algorithm is more involved and has not been implemented here. That
    might be interesting future work.  *)
