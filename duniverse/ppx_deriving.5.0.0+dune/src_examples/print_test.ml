
type t =
  | Leaf of int
  | Node of t list
  [@@deriving show]

let test_case =
  (Node
     [(Node
         [(Node
             [(Node
                 [(Node
                     [(Node [(Node [(Leaf 81)])]);
                      (Node
                         [(Node
                             [(Leaf 43);
                              (Node
                                 [(Leaf 71); (Leaf 75);
                                  (Leaf 92)]);
                              (Node
                                 [(Node
                                     [(Leaf 63); (Leaf 82);
                                      (Leaf 69); (Leaf 95)])])])])]);
                  (Node
                     [(Node
                         [(Leaf 30); (Leaf 63); (Leaf 36);
                          (Leaf 67)])])]);
              (Node
                 [(Leaf 15);
                  (Node
                     [(Leaf 94); (Leaf 93);
                      (Node
                         [(Leaf 62); (Leaf 76); (Leaf 4);
                          (Leaf 37)])])]);
              (Node
                 [(Node
                     [(Node
                         [(Node
                             [(Leaf 23);
                              (Node
                                 [(Node [(Leaf 59); (Leaf 58)]);
                                  (Leaf 4)]); (Leaf 9);
                              (Leaf 90)]);
                          (Node
                             [(Leaf 42);
                              (Node
                                 [(Leaf 54);
                                  (Node
                                     [(Leaf 49); (Leaf 96);
                                      (Leaf 64); (Leaf 96)])])]);
                          (Leaf 16);
                          (Node
                             [(Leaf 23); (Leaf 11);
                              (Node
                                 [(Leaf 66); (Leaf 4);
                                  (Leaf 29); (Leaf 92)]);
                              (Leaf 45)])]);
                      (Node
                         [(Leaf 73); (Node [(Leaf 83)]);
                          (Leaf 49)])]);
                  (Node
                     [(Node [(Leaf 80); (Leaf 43)]);
                      (Leaf 50); (Node [(Leaf 87)])]);
                  (Node [(Leaf 48)]);
                  (Node
                     [(Node
                         [(Leaf 79); (Leaf 73); (Leaf 8);
                          (Leaf 24)]); (Leaf 49); (Leaf 53)])]);
              (Node
                 [(Node
                     [(Node
                         [(Node
                             [(Node
                                 [(Node
                                     [(Node
                                         [(Leaf 61); (Leaf 20)])]);
                                  (Leaf 27); (Leaf 30);
                                  (Node [(Leaf 4)])])])]);
                      (Node
                         [(Node
                             [(Node
                                 [(Node
                                     [(Node
                                         [(Node
                                             [(Leaf 6); (Leaf 33);
                                              (Leaf 80)]);
                                          (Leaf 19)]); (Leaf 28)])])]);
                          (Leaf 4); (Leaf 65); (Leaf 1)]);
                      (Node [(Leaf 22); (Leaf 93)]);
                      (Leaf 65)])])]);
          (Node
             [(Node
                 [(Node
                     [(Node
                         [(Leaf 63);
                          (Node
                             [(Node
                                 [(Node
                                     [(Node
                                         [(Leaf 79); (Leaf 2);
                                          (Node
                                             [(Leaf 66); (Leaf 53)]);
                                          (Node
                                             [(Leaf 7); (Leaf 42);
                                              (Leaf 31);
                                              (Node
                                                 [(Leaf 58);
                                                  (Leaf 87);
                                                  (Leaf 52)])])]);
                                      (Node
                                         [(Leaf 37); (Leaf 74);
                                          (Node
                                             [(Leaf 43); (Leaf 98);
                                              (Leaf 28); (Leaf 52)]);
                                          (Leaf 50)])])]);
                              (Leaf 98)]);
                          (Node
                             [(Leaf 77); (Node [(Leaf 79)]);
                              (Node
                                 [(Node
                                     [(Leaf 17); (Leaf 4);
                                      (Leaf 21); (Leaf 34)]);
                                  (Leaf 64);
                                  (Node
                                     [(Node
                                         [(Node
                                             [(Leaf 31); (Leaf 60);
                                              (Node
                                                 [(Node
                                                     [(Node
                                                         [(Node
                                                             [(Leaf 14);
                                                              (Leaf 11);
                                                              (Leaf 27);
                                                              (Leaf 43)]);
                                                          (Leaf 21)]);
                                                      (Leaf 3)])])])])]);
                                  (Node
                                     [(Leaf 93); (Leaf 3);
                                      (Leaf 37)])])])]);
                      (Node
                         [(Node
                             [(Node
                                 [(Node
                                     [(Leaf 37);
                                      (Node
                                         [(Node
                                             [(Node
                                                 [(Leaf 59);
                                                  (Leaf 37)])]);
                                          (Leaf 98)])])]);
                              (Node
                                 [(Node
                                     [(Leaf 54); (Leaf 72);
                                      (Leaf 21)]);
                                  (Node [(Leaf 87); (Leaf 25)])]);
                              (Node
                                 [(Leaf 45);
                                  (Node
                                     [(Node
                                         [(Leaf 35); (Leaf 72);
                                          (Leaf 14)]); (Leaf 93);
                                      (Node
                                         [(Node [(Leaf 75)])])]);
                                  (Node
                                     [(Leaf 30);
                                      (Node [(Leaf 21)]);
                                      (Node
                                         [(Leaf 0); (Leaf 5);
                                          (Node
                                             [(Leaf 97); (Leaf 15)])]);
                                      (Node [(Leaf 55)])])])])]);
                      (Leaf 34);
                      (Node
                         [(Node
                             [(Node
                                 [(Node
                                     [(Node
                                         [(Leaf 90); (Leaf 54);
                                          (Node
                                             [(Leaf 34);
                                              (Node
                                                 [(Leaf 80);
                                                  (Leaf 45)])])])]);
                                  (Node
                                     [(Node
                                         [(Leaf 87); (Leaf 92);
                                          (Node
                                             [(Node
                                                 [(Leaf 61);
                                                  (Node
                                                     [(Leaf 96);
                                                      (Leaf 19)])]);
                                              (Leaf 25)]);
                                          (Leaf 32)])])])]);
                          (Node
                             [(Node
                                 [(Node
                                     [(Leaf 80); (Leaf 4);
                                      (Leaf 15)]);
                                  (Node
                                     [(Leaf 37);
                                      (Node [(Leaf 77)]);
                                      (Leaf 74); (Leaf 52)]);
                                  (Leaf 98)])])])]);
                  (Node
                     [(Node
                         [(Node
                             [(Leaf 53); (Leaf 56);
                              (Node
                                 [(Leaf 21); (Leaf 48);
                                  (Leaf 63); (Leaf 58)]);
                              (Leaf 12)]);
                          (Node
                             [(Leaf 43); (Leaf 11);
                              (Node [(Leaf 84); (Leaf 50)])]);
                          (Node
                             [(Node
                                 [(Leaf 6);
                                  (Node
                                     [(Leaf 13); (Leaf 28);
                                      (Leaf 80)]);
                                  (Node [(Leaf 5); (Leaf 37)])]);
                              (Leaf 5); (Leaf 31); (Leaf 51)]);
                          (Leaf 59)]);
                      (Node
                         [(Node
                             [(Leaf 15);
                              (Node
                                 [(Leaf 55); (Leaf 25);
                                  (Leaf 58); (Leaf 0)]);
                              (Leaf 88)]);
                          (Node
                             [(Node [(Leaf 37)]);
                              (Node
                                 [(Node
                                     [(Leaf 0); (Leaf 40);
                                      (Node
                                         [(Leaf 28); (Leaf 24);
                                          (Node
                                             [(Leaf 99); (Leaf 70)]);
                                          (Node
                                             [(Leaf 86); (Leaf 52);
                                              (Leaf 72); (Leaf 41)])])]);
                                  (Node
                                     [(Node
                                         [(Leaf 16);
                                          (Node
                                             [(Leaf 87); (Leaf 16);
                                              (Leaf 61)])])])]);
                              (Node
                                 [(Node
                                     [(Leaf 13); (Leaf 51);
                                      (Leaf 78);
                                      (Node
                                         [(Leaf 69); (Leaf 8);
                                          (Leaf 70)])]); (Leaf 8);
                                  (Leaf 13); (Leaf 22)]);
                              (Node
                                 [(Leaf 98); (Leaf 25);
                                  (Leaf 14); (Leaf 28)])])]);
                      (Node
                         [(Node
                             [(Leaf 62);
                              (Node
                                 [(Leaf 65);
                                  (Node
                                     [(Leaf 13); (Leaf 78);
                                      (Leaf 52)]); (Leaf 26);
                                  (Leaf 64)])])])])]);
              (Node
                 [(Leaf 18); (Node [(Leaf 50)]);
                  (Leaf 84)]);
              (Node
                 [(Node
                     [(Node
                         [(Leaf 2); (Leaf 18);
                          (Node [(Leaf 75)])]); (Leaf 15)]);
                  (Node [(Leaf 69); (Leaf 66)]);
                  (Node [(Leaf 59); (Node [(Leaf 84)])]);
                  (Node
                     [(Leaf 57);
                      (Node
                         [(Node
                             [(Node
                                 [(Node
                                     [(Leaf 40); (Leaf 9);
                                      (Leaf 1)])]);
                              (Node
                                 [(Leaf 81); (Leaf 82);
                                  (Node
                                     [(Leaf 86); (Leaf 14);
                                      (Leaf 67); (Leaf 58)])]);
                              (Node [(Leaf 25)]);
                              (Node
                                 [(Leaf 53);
                                  (Node
                                     [(Leaf 97); (Leaf 48);
                                      (Leaf 90)])])]); (Leaf 97);
                          (Node
                             [(Node [(Leaf 80); (Leaf 28)])]);
                          (Node
                             [(Leaf 61); (Leaf 79); (Leaf 60);
                              (Leaf 81)])]);
                      (Node
                         [(Node [(Node [(Leaf 5)])]);
                          (Node
                             [(Node
                                 [(Node [(Leaf 89)]);
                                  (Node
                                     [(Leaf 41); (Leaf 79);
                                      (Leaf 47)]); (Leaf 78);
                                  (Node
                                     [(Leaf 20); (Leaf 39);
                                      (Node [(Leaf 54)])])])]);
                          (Node
                             [(Node [(Leaf 55)]);
                              (Node
                                 [(Leaf 64); (Leaf 45);
                                  (Leaf 92); (Leaf 45)])])])])]);
              (Node
                 [(Node
                     [(Node [(Leaf 58)]);
                      (Node
                         [(Leaf 63); (Leaf 47);
                          (Node
                             [(Leaf 34);
                              (Node
                                 [(Leaf 39);
                                  (Node
                                     [(Leaf 69); (Leaf 10);
                                      (Leaf 24)]);
                                  (Node
                                     [(Leaf 20); (Leaf 32);
                                      (Leaf 12); (Leaf 9)])]);
                              (Leaf 46)])]); (Leaf 29);
                      (Node
                         [(Leaf 78); (Leaf 35); (Leaf 15)])]);
                  (Node
                     [(Node
                         [(Node
                             [(Leaf 27); (Leaf 56); (Leaf 21);
                              (Leaf 89)]);
                          (Node
                             [(Leaf 69);
                              (Node
                                 [(Node [(Leaf 94)]);
                                  (Leaf 21)]);
                              (Node
                                 [(Leaf 76); (Leaf 35);
                                  (Node
                                     [(Leaf 39); (Leaf 40);
                                      (Leaf 52)]); (Leaf 21)])])]);
                      (Leaf 97);
                      (Node [(Leaf 30); (Leaf 93)])]);
                  (Node [(Leaf 38)]);
                  (Node
                     [(Leaf 34);
                      (Node
                         [(Leaf 13); (Leaf 1);
                          (Node [(Leaf 44); (Leaf 93)])])])])])])])

let () =
  Format.printf "tree: %a@." pp test_case;
  ()
