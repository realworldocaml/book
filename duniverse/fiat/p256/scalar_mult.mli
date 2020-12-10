val scalar_mult : Scalar.t -> Point.t -> Point.t
(** Scalar multiplication, implemented using the Montgomery powering ladder.
    @see <https://cr.yp.to/bib/2003/joye-ladder.pdf> *)
