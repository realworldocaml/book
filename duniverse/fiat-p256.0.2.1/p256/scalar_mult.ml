let scalar_mult d p =
  let r0 = ref (Point.at_infinity ()) in
  let r1 = ref p in
  for i = 255 downto 0 do
    let bit = Scalar.bit_at d i in
    let sum = Point.add !r0 !r1 in
    let r0_double = Point.double !r0 in
    let r1_double = Point.double !r1 in
    r0 := Point.select bit ~then_:sum ~else_:r0_double;
    r1 := Point.select bit ~then_:r1_double ~else_:sum
  done;
  !r0
