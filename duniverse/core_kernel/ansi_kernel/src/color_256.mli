(** Support for 256-color handling on terminals/consoles.  Note that the
    functions [of_rgb6_exn] and [of_rgb6] return values within the 6x6x6
    color-cube space, even though equivalent duplicates exist in the first 16
    and last 24 colors (a "best-fit-equivalent" function could be added some
    day, if there ever becomes a requirement for it -- see note 2 below).

    NOTE 1: the color-cube is not linear in terms of RGB levels, but is
    "shifted" towards brighter values (as opposed to a logarithmic or other
    mapping). Visually, the "rgb6" values 0 to 5 (used in [of_rgb6_exn] and
    encoded in the 256-color palette values) map into RGB levels as follows:

    {v
    Cube-val:  0           1    2    3    4    5
       v       +-----------+----+----+----+----+
      RGB:     0           95   135  175  215  255
    v}

    The floating-point values used in [of_rgb] are {e not} weighted in the same
    way, but are rounded to the nearest cube-value -- a value of 0.5 (50% or
    127.5/255) would result in a color-cube value of 2, for example.  Any level
    less than 0.187 (approx.) maps to 0 (black) and greater than 0.921 to 5
    (white).

    NOTE 2: is is {e not} recommended to use the 256-color palette for
    representing the 16 (8 * 2) 'primary' colors.  The standard [`Black],
    [`Blue], etc. attributes are recommended for these.
*)

type t [@@deriving sexp_of, compare, hash, equal]

val to_int : t -> int
val of_int_exn : int -> t

(** Takes an RGB triple with values in the range [0,5] (inclusive) and returns
    the appropriate 256-color palette value.  Will throw an exception if any of
    the inputs are out-of-range.  Note that the input values are weighted as
    described above. *)
val of_rgb6_exn : int * int * int -> t

(** Takes an RGB triple with float values in the closed (inclusive) interval
    [0,1] and returns the nearest (rounded) 256-color palette value.
    Out-of-bound values are clamped to the range.  The inputs are {e not}
    weighted, unlike [of_rgb6_exn] as described above. *)
val of_rgb : float * float * float -> t

(** Takes an RGB triple with integer values in the closed (inclusive) range
    [0,255] and returns the nearest (rounded) 256-color palette color-cube
    value.  Out-of-bound values are clamped to the range.  The inputs are
    {e not} weighted, unlike [of_rgb6_exn] as described above. *)
val of_rgb_8bit : int * int * int -> t

(** Takes an RGB triple with integer values in the closed (inclusive) range
    [0,1000] and returns the nearest (rounded) 256-color palette color-cube
    value.  Out-of-bound values are clamped to the range.  The inputs are {e not}
    weighted, unlike [of_rgb6_exn] as described above. *)
val of_rgb_int1k : int * int * int -> t

(** Takes a grayscale level from [0-23] (inclusive, not-black-to-not-white)
    and returns the appropriate 256-color palette value.  Will throw an
    exception if the input value is out-of-range. *)
val of_gray24_exn : int -> t

(** Takes a color palette value and returns, for color-cube and grayscale
    palette values, an approximated [`RGB] triple with each component in the
    range [0,1];  for the first 16 values, [`Primary] followed by the specific
    palette index is returned, as these are not consistently defined. *)
val to_rgb : t -> [> `Primary of int | `RGB of float * float * float ]

(** Takes a color palette value and returns a hex-encoded RGB 24-bit triplet,
    with a leading '#'.  I.e. "#000000" through "#ffffff".  The values
    returned for the first 16 (primary) colors follow the Windows Console
    scheme, as documented here: https://en.wikipedia.org/wiki/ANSI_escape_code .
*)
val to_rgb_hex24 : t -> string

(** Takes a color palette value and returns an approximate luminance value
    in the closed interval [0,1]. *)
val to_luma : t -> float

(** Takes a color palette value and returns a triple of RGB integers in
    the closed interval [0,255]. *)
val to_rgb_8bit : t -> int * int * int

(** Takes a color palette value and returns a triple of RGB integers in
    the closed interval [0,1000]. *)
val to_rgb_int1k : t -> int * int * int

(** Takes a color palette value and returns a triple of RGB integers in
    the closed interval [0,5].  For color-cube palette values, this simply
    un-does [of_rgb6_exn].  For others, it will return the closest matching
    value in the color-cube. *)
val to_rgb6 : t -> int * int * int
