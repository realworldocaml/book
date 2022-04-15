open HopixAST
open Position

type unit_or_def =
  | Unit of expression located
  | Def of value_definition located

let expr_of_unit_or_def_list (l : unit_or_def list) e =
  let rec aux l : expression located =
    match l with
    | [] -> e
    | Unit e1 :: xs ->
        let e2 = aux xs in
        {
          value =
            Sequence
              (match e2.value with Sequence l -> e1 :: l | _ -> [ e1; e2 ]);
          position = join e1.position e2.position;
        }
    | Def d :: xs ->
        let e2 = aux xs in
        { value = Define (d.value, e2); position = join d.position e2.position }
  in
  (aux l).value
