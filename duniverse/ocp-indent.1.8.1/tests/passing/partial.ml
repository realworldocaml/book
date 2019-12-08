   let () =
 ffff;
     hhhhhh;
        fff;
 let (quot, _rem) =
   let quot_rem n k =
     let (d, m) = (n / k, n mod k) in
     if d < 0 && m > 0 then (d+1, m-k)
else (d, m)
    in
    let quot n k = fst (quot_rem n k) in
    let rem n k = snd (quot_rem n k) in

quot, rem
