module T = struct
  type 'a t = 'a list * 'a list

  let empty = [],[]

  let enqueue (l1,l2) x =
    (x :: l1,l2)

  let rec dequeue (in_list,out_list) =
    match out_list with
    | hd :: tl -> Some (hd, (in_list,tl))
    | [] -> dequeue ([], List.rev in_list)

  let fold (in_list,out_list) ~init ~f =
    List.fold ~init:(List.fold ~init ~f out_list)
      ~f (List.rev in_list)
end
include T
include Foldable.Extend(T
