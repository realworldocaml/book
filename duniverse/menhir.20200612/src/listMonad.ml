(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

type 'a m = 'a list

let return x =
  [ x ]

let bind l f =
  List.flatten (List.map f l)

let ( >>= ) l f =
  bind l f

(*
   1. (return x) >>= f == f x

   bind [ x ] f
   = List.flatten (List.map f [ x ])
   = f x

   2. m >>= return == m

   bind l return
   = List.flatten (List.map (fun x -> [ x ]) (x1::x2::..::xn))
   = List.flatten ([x1]::...::[xn])
   = x1::...::xn
   = l

   3. (m >>= f) >>= g == m >>= (\x -> f x >>= g)

   bind (bind l f) g
   = List.flatten (List.map g (List.flatten (List.map f (x1::...::xn))))
   = List.flatten (List.map g (f x1 :: f x2 :: ... :: f xn))
   = List.flatten (List.map g ([fx1_1; fx1_2 ... ] :: [fx2_1; ... ] :: ...))
   = List.flatten ([ g fx1_1; g fx_1_2 ... ] :: [ g fx_2_1; ... ] ...)
   = List.flatten (List.map (fun x -> List.flatten (List.map g (f x))) l)
   = bind l (fun x -> bind (f x) g)

*)

