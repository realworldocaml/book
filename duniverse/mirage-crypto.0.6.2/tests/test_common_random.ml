let sample arr =
  let ix = Randomconv.int ~bound:(Array.length arr) Mirage_crypto_rng.generate in
  arr.(ix)
