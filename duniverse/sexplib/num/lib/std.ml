
module Big_int = struct
  include Big_int

  let sexp_of_big_int = Sexplib_num_conv.sexp_of_big_int
  let big_int_of_sexp = Sexplib_num_conv.big_int_of_sexp
end

module Nat = struct
  include Nat

  let sexp_of_nat = Sexplib_num_conv.sexp_of_nat
  let nat_of_sexp = Sexplib_num_conv.nat_of_sexp
end

module Ratio = struct
  include Ratio

  let sexp_of_ratio = Sexplib_num_conv.sexp_of_ratio
  let ratio_of_sexp = Sexplib_num_conv.ratio_of_sexp
end

module Num = struct
  include Num

  let sexp_of_num = Sexplib_num_conv.sexp_of_num
  let num_of_sexp = Sexplib_num_conv.num_of_sexp
end
