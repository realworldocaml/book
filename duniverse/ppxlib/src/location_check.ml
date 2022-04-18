open Import

module Non_intersecting_ranges : sig
  type t

  val empty : t
  val insert : node_name:string -> Location.t -> t -> t
  val union : t -> t -> t

  val covered_by : t -> loc:Location.t -> bool
  (** [covered_by t ~loc = true] iff [t] is covered by [loc] *)

  val find_outside : Location.t -> t -> string * Location.t
end = struct
  type t = {
    min_pos : Lexing.position option;
    max_pos : Lexing.position option;
    ranges : (string * Location.t) list;
  }

  let empty = { min_pos = None; max_pos = None; ranges = [] }

  let rec insert ranges ((node_name, node_loc) as node) =
    match ranges with
    | [] -> [ node ]
    | ((x_name, x_loc) as x) :: xs ->
        let open Location in
        if compare_pos node_loc.loc_start x_loc.loc_end >= 0 then
          node :: x :: xs
        else if compare_pos x_loc.loc_start node_loc.loc_end >= 0 then
          x :: insert xs node
        else
          raise_errorf ~loc:node_loc
            "invalid output from ppx, %s overlaps with %s at location:@.%a"
            node_name x_name Location.print x_loc

  let min_pos p1 p2 =
    match (p1, p2) with
    | None, None -> None
    | (Some _ as p), None | None, (Some _ as p) -> p
    | Some p1, Some p2 -> Some (Location.min_pos p1 p2)

  let max_pos p1 p2 =
    match (p1, p2) with
    | None, None -> None
    | (Some _ as p), None | None, (Some _ as p) -> p
    | Some p1, Some p2 -> Some (Location.max_pos p1 p2)

  let longest_first l1 l2 ~stop_after =
    let rec loop xs ys n =
      match (xs, ys, n) with
      | [], _, _ | _, _, 0 -> (l2, l1)
      | _, [], _ -> (l1, l2)
      | _ :: xs, _ :: ys, n -> loop xs ys (n - 1)
    in
    loop l1 l2 stop_after

  let union t1 t2 =
    let init, l = longest_first t1.ranges t2.ranges ~stop_after:42 in
    let ranges = List.fold_left l ~init ~f:insert in
    {
      min_pos = min_pos t1.min_pos t2.min_pos;
      max_pos = max_pos t1.max_pos t2.max_pos;
      ranges;
    }

  let insert ~node_name loc t =
    {
      min_pos = min_pos (Some loc.loc_start) t.min_pos;
      max_pos = max_pos (Some loc.loc_end) t.max_pos;
      ranges = insert t.ranges (node_name, loc);
    }

  let covered_by t ~loc =
    match (t.min_pos, t.max_pos) with
    | None, None -> true
    | Some min_pos, Some max_pos ->
        Location.compare_pos min_pos loc.loc_start >= 0
        && Location.compare_pos max_pos loc.loc_end <= 0
    | _, _ ->
        (* there are no open ranges *)
        assert false

  let find_outside loc t =
    List.find t.ranges ~f:(fun (_, l) ->
        Location.compare_pos loc.loc_start l.loc_start > 0
        || Location.compare_pos loc.loc_end l.loc_end < 0)
end

let reloc_pmty_functors x =
  let outmost_loc = x.pmty_loc in
  let rec aux x =
    match x.pmty_desc with
    | Pmty_functor (Unit, initial_res) ->
        let res = aux initial_res in
        if res == initial_res then x
        else { x with pmty_desc = Pmty_functor (Unit, res) }
    | Pmty_functor (Named (id, mty), initial_res) ->
        let res = aux initial_res in
        if Location.compare outmost_loc res.pmty_loc = 0 then
          let loc_start = mty.pmty_loc.loc_end in
          let res = { res with pmty_loc = { res.pmty_loc with loc_start } } in
          { x with pmty_desc = Pmty_functor (Named (id, mty), res) }
        else if res == initial_res then x
        else { x with pmty_desc = Pmty_functor (Named (id, mty), res) }
    | _ -> x
  in
  aux x

let reloc_pmod_functors x =
  let outmost_loc = x.pmod_loc in
  let rec aux x =
    match x.pmod_desc with
    | Pmod_functor (Unit, initial_res) ->
        let res = aux initial_res in
        if res == initial_res then x
        else { x with pmod_desc = Pmod_functor (Unit, res) }
    | Pmod_functor (Named (id, mty), initial_res) ->
        let res = aux initial_res in
        if Location.compare outmost_loc res.pmod_loc = 0 then
          let loc_start = mty.pmty_loc.loc_end in
          let res = { res with pmod_loc = { res.pmod_loc with loc_start } } in
          { x with pmod_desc = Pmod_functor (Named (id, mty), res) }
        else if res == initial_res then x
        else { x with pmod_desc = Pmod_functor (Named (id, mty), res) }
    | _ -> x
  in
  aux x

let all_payloads_inside_parent ~loc =
  List.for_all ~f:(fun attr ->
      Location.compare_pos loc.loc_end attr.attr_loc.loc_end >= 0)

let file : string option ref = ref None
let same_file_so_far = ref true

let stayed_in_the_same_file fname =
  (* TODO: remove uses of Location.none from the ppxes. *)
  if String.equal fname "_none_" then true (* do nothing for now. *)
  else
    match !file with
    | None ->
        file := Some fname;
        true
    | Some orig_fname ->
        String.equal orig_fname fname
        ||
        (same_file_so_far := false;
         false)

let should_ignore loc attrs =
  (* If the filename changed, then there were line directives, and the locations
     are all messed up. *)
  (not (stayed_in_the_same_file loc.loc_start.pos_fname))
  || (* Ignore things explicitly marked. *)
  List.exists
    ~f:(fun attr ->
      String.equal attr.attr_name.txt
        Merlin_helpers.hide_attribute.attr_name.txt)
    attrs

let rec extract_constraint e =
  match e.pexp_desc with
  | Pexp_constraint (e, ct) | Pexp_coerce (e, None, ct) -> Some (e, ct)
  | Pexp_newtype (name, exp) ->
      Option.map (extract_constraint exp) ~f:(fun (exp, ct) ->
          ( {
              e with
              pexp_desc = Pexp_newtype (name, exp);
              pexp_loc = { e.pexp_loc with loc_ghost = true };
            },
            ct ))
  | _ -> None

let do_check ~node_name node_loc childrens_locs siblings_locs =
  if not !same_file_so_far then Non_intersecting_ranges.empty
  else if node_loc.loc_ghost then
    Non_intersecting_ranges.union childrens_locs siblings_locs
  else if Non_intersecting_ranges.covered_by childrens_locs ~loc:node_loc then
    Non_intersecting_ranges.insert ~node_name node_loc siblings_locs
  else
    let child_name, child_loc =
      Non_intersecting_ranges.find_outside node_loc childrens_locs
    in
    Location.raise_errorf ~loc:node_loc
      "invalid output from ppx:@ this %s is built from a%s whose location is \
       outside of this node's.@.Child %s found at:@ %a"
      node_name
      ((match String.unsafe_get child_name 0 with
       | 'a' | 'e' | 'i' | 'o' | 'u' -> "n "
       | _ -> " ")
      ^ child_name)
      child_name Location.print child_loc

let enforce_invariants fname =
  let () = file := fname in
  object (self)
    inherit [Non_intersecting_ranges.t] Ast_traverse.fold as super

    (* TODO: we should generate a class which enforces the location invariant.
       And then we should only override the methods where we need an escape
       hatch because the parser isn't doing the right thing.

       That would ensure that we stay up to date as the AST changes. *)

    method! longident_loc x siblings =
      if x.loc.loc_ghost then siblings
      else Non_intersecting_ranges.insert ~node_name:"ident" x.loc siblings

    method! row_field x siblings_locs =
      if should_ignore x.prf_loc x.prf_attributes then siblings_locs
      else
        let childrens_locs = super#row_field x Non_intersecting_ranges.empty in
        do_check ~node_name:"row field" x.prf_loc childrens_locs siblings_locs

    method! object_field x siblings_locs =
      if should_ignore x.pof_loc x.pof_attributes then siblings_locs
      else
        let childrens_locs =
          super#object_field x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"object field" x.pof_loc childrens_locs
          siblings_locs

    method! binding_op x siblings_locs =
      let childrens_locs = super#binding_op x Non_intersecting_ranges.empty in
      do_check ~node_name:"binding operator" x.pbop_loc childrens_locs
        siblings_locs

    method! value_description x siblings_locs =
      if should_ignore x.pval_loc x.pval_attributes then siblings_locs
      else
        let childrens_locs =
          super#value_description x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"value description" x.pval_loc childrens_locs
          siblings_locs

    method! type_declaration x siblings_locs =
      if should_ignore x.ptype_loc x.ptype_attributes then siblings_locs
      else
        let childrens_locs =
          super#type_declaration x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"type declaration" x.ptype_loc childrens_locs
          siblings_locs

    method! label_declaration x siblings_locs =
      if should_ignore x.pld_loc x.pld_attributes then siblings_locs
      else
        let childrens_locs =
          super#label_declaration x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"label declaration" x.pld_loc childrens_locs
          siblings_locs

    method! constructor_declaration x siblings_locs =
      if should_ignore x.pcd_loc x.pcd_attributes then siblings_locs
      else
        let childrens_locs =
          super#constructor_declaration x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"constructor declaration" x.pcd_loc childrens_locs
          siblings_locs

    method! type_extension x siblings_locs =
      if should_ignore x.ptyext_loc x.ptyext_attributes then siblings_locs
      else
        let childrens_locs =
          super#type_extension x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"type extension" x.ptyext_loc childrens_locs
          siblings_locs

    method! extension_constructor x siblings_locs =
      if should_ignore x.pext_loc x.pext_attributes then siblings_locs
      else
        let childrens_locs =
          super#extension_constructor x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"extension constructor" x.pext_loc childrens_locs
          siblings_locs

    method! class_type x siblings_locs =
      if should_ignore x.pcty_loc x.pcty_attributes then siblings_locs
      else
        let childrens_locs = super#class_type x Non_intersecting_ranges.empty in
        do_check ~node_name:"class type" x.pcty_loc childrens_locs siblings_locs

    method! class_type_field x siblings_locs =
      if should_ignore x.pctf_loc x.pctf_attributes then siblings_locs
      else
        let childrens_locs =
          super#class_type_field x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"class type field" x.pctf_loc childrens_locs
          siblings_locs

    method! class_infos f x siblings_locs =
      if should_ignore x.pci_loc x.pci_attributes then siblings_locs
      else
        let childrens_locs =
          super#class_infos f x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"class" x.pci_loc childrens_locs siblings_locs

    method! class_expr x siblings_locs =
      if should_ignore x.pcl_loc x.pcl_attributes then siblings_locs
      else
        let childrens_locs = super#class_expr x Non_intersecting_ranges.empty in
        do_check ~node_name:"class expression" x.pcl_loc childrens_locs
          siblings_locs

    method! class_field x siblings_locs =
      if should_ignore x.pcf_loc x.pcf_attributes then siblings_locs
      else
        let childrens_locs =
          super#class_field x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"class field" x.pcf_loc childrens_locs siblings_locs

    method! signature_item x siblings_locs =
      if should_ignore x.psig_loc [] then siblings_locs
      else
        let childrens_locs =
          super#signature_item x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"signature item" x.psig_loc childrens_locs
          siblings_locs

    method! module_declaration x siblings_locs =
      if should_ignore x.pmd_loc x.pmd_attributes then siblings_locs
      else
        let childrens_locs =
          super#module_declaration x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"module declaration" x.pmd_loc childrens_locs
          siblings_locs

    method! module_substitution x siblings_locs =
      if should_ignore x.pms_loc x.pms_attributes then siblings_locs
      else
        let childrens_locs =
          super#module_substitution x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"module substitution" x.pms_loc childrens_locs
          siblings_locs

    method! module_type_declaration x siblings_locs =
      if should_ignore x.pmtd_loc x.pmtd_attributes then siblings_locs
      else
        let childrens_locs =
          super#module_type_declaration x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"module type declaration" x.pmtd_loc childrens_locs
          siblings_locs

    method! open_infos f x siblings_locs =
      if should_ignore x.popen_loc x.popen_attributes then siblings_locs
      else
        let childrens_locs =
          super#open_infos f x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"open" x.popen_loc childrens_locs siblings_locs

    method! include_infos f x siblings_locs =
      if should_ignore x.pincl_loc x.pincl_attributes then siblings_locs
      else
        let childrens_locs =
          super#include_infos f x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"include" x.pincl_loc childrens_locs siblings_locs

    method! structure_item x siblings_locs =
      if should_ignore x.pstr_loc [] then siblings_locs
      else
        let childrens_locs =
          super#structure_item x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"structure item" x.pstr_loc childrens_locs
          siblings_locs

    method! module_binding x siblings_locs =
      if should_ignore x.pmb_loc x.pmb_attributes then siblings_locs
      else
        let childrens_locs =
          super#module_binding x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"module binding" x.pmb_loc childrens_locs
          siblings_locs

    (******************************************)
    (* The following is special cased because *)
    (* the type constraint is duplicated.     *)
    (******************************************)

    method! value_binding x siblings_locs =
      if should_ignore x.pvb_loc x.pvb_attributes then siblings_locs
      else
        let childrens_locs =
          match (x.pvb_pat.ppat_desc, extract_constraint x.pvb_expr) with
          (* let x : type a b c. ct = e *)
          | ( Ppat_constraint
                (pvb_pat, { ptyp_desc = Ptyp_poly (_ :: _, ctp); _ }),
              Some (pvb_expr, cte) )
          (* let x :  ct = e
             let x :> ct = e *)
          | ( Ppat_constraint (pvb_pat, { ptyp_desc = Ptyp_poly ([], ctp); _ }),
              Some (pvb_expr, cte) )
            when Location.compare ctp.ptyp_loc cte.ptyp_loc = 0 ->
              let acc = Non_intersecting_ranges.empty in
              let acc = self#pattern pvb_pat acc in
              let _acc = self#core_type ctp acc in
              let acc = self#expression pvb_expr acc in
              let acc = self#attributes x.pvb_attributes acc in
              acc
          | _ -> super#value_binding x Non_intersecting_ranges.empty
        in
        do_check ~node_name:"value binding" x.pvb_loc childrens_locs
          siblings_locs

    (**********************************************)
    (* The following is special cased because of: *)
    (*     MT [@attr payload]                     *)
    (* where the loc of payload is outside the    *)
    (* loc of the module type....                 *)
    (* and                                        *)
    (*     functor (A : S) (B : S) ...            *)
    (* where the loc of [(B : S) ...] is the same *)
    (* as the loc of the outermost module type.   *)
    (**********************************************)

    method! module_type x siblings_locs =
      if should_ignore x.pmty_loc x.pmty_attributes then siblings_locs
      else
        let x = reloc_pmty_functors x in
        let childrens_locs =
          if all_payloads_inside_parent ~loc:x.pmty_loc x.pmty_attributes then
            super#module_type x Non_intersecting_ranges.empty
          else
            let acc =
              self#module_type_desc x.pmty_desc Non_intersecting_ranges.empty
            in
            let _ = self#attributes x.pmty_attributes acc in
            acc
        in
        do_check ~node_name:"module type" x.pmty_loc childrens_locs
          siblings_locs

    (**********************************************)
    (* The following is special cased because of: *)
    (*     ME [@attr payload]                     *)
    (* where the loc of payload is outside the    *)
    (* loc of the module expr....                 *)
    (* and                                        *)
    (*     functor (A : S) (B : S) ...            *)
    (* where the loc of [(B : S) ...] is the same *)
    (* as the loc of the outermost module expr.   *)
    (**********************************************)

    method! module_expr x siblings_locs =
      if should_ignore x.pmod_loc x.pmod_attributes then siblings_locs
      else
        let x = reloc_pmod_functors x in
        let childrens_locs =
          if all_payloads_inside_parent ~loc:x.pmod_loc x.pmod_attributes then
            super#module_expr x Non_intersecting_ranges.empty
          else
            let acc =
              self#module_expr_desc x.pmod_desc Non_intersecting_ranges.empty
            in
            let _ = self#attributes x.pmod_attributes acc in
            acc
        in
        do_check ~node_name:"module expression" x.pmod_loc childrens_locs
          siblings_locs

    (*********************)
    (* Same as above ... *)
    (*********************)

    method! core_type x siblings_locs =
      if should_ignore x.ptyp_loc x.ptyp_attributes then siblings_locs
      else
        let childrens_locs =
          if all_payloads_inside_parent ~loc:x.ptyp_loc x.ptyp_attributes then
            super#core_type x Non_intersecting_ranges.empty
          else
            let acc =
              self#core_type_desc x.ptyp_desc Non_intersecting_ranges.empty
            in
            let _ = self#attributes x.ptyp_attributes acc in
            acc
        in
        do_check ~node_name:"core type" x.ptyp_loc childrens_locs siblings_locs

    (*****************)
    (* And again ... *)
    (*****************)

    method! expression x siblings_locs =
      if should_ignore x.pexp_loc x.pexp_attributes then siblings_locs
      else
        let childrens_locs =
          if all_payloads_inside_parent ~loc:x.pexp_loc x.pexp_attributes then
            super#expression x Non_intersecting_ranges.empty
          else
            let acc =
              self#expression_desc x.pexp_desc Non_intersecting_ranges.empty
            in
            let _ = self#attributes x.pexp_attributes acc in
            acc
        in
        do_check ~node_name:"expression" x.pexp_loc childrens_locs siblings_locs

    (*****************)
    (* ... and again *)
    (*****************)

    method! pattern x siblings_locs =
      if should_ignore x.ppat_loc x.ppat_attributes then siblings_locs
      else
        let childrens_locs =
          if all_payloads_inside_parent ~loc:x.ppat_loc x.ppat_attributes then
            super#pattern x Non_intersecting_ranges.empty
          else
            let acc =
              self#pattern_desc x.ppat_desc Non_intersecting_ranges.empty
            in
            let _ = self#attributes x.ppat_attributes acc in
            acc
        in
        do_check ~node_name:"pattern" x.ppat_loc childrens_locs siblings_locs

    (***********************************************************)
    (* The following is special cased because the location of  *)
    (* the construct equals the location of the type_exception *)
    (* (and so covers the location of the attributes).         *)
    (***********************************************************)

    method! type_exception x siblings_locs =
      if should_ignore x.ptyexn_loc x.ptyexn_attributes then siblings_locs
      else
        let init = Non_intersecting_ranges.empty in
        let childs_locs =
          self#extension_constructor x.ptyexn_constructor init
        in
        let attrs_locs = self#attributes x.ptyexn_attributes init in
        ignore
          (do_check ~node_name:"exception" x.ptyexn_loc attrs_locs siblings_locs);
        do_check ~node_name:"exception" x.ptyexn_loc childs_locs siblings_locs

    (*******************************************)
    (* The following is overridden because the *)
    (* lhs is sometimes included in the rhs.   *)
    (*******************************************)

    method! with_constraint x siblings_loc =
      match x with
      | Pwith_type (_, tdecl) | Pwith_typesubst (_, tdecl) ->
          self#type_declaration tdecl siblings_loc
      | _ -> super#with_constraint x siblings_loc

    (******************************************)
    (* The following is overridden because of:*)
    (* - Foo.{ bar; ... }                     *)
    (* - Foo.[ bar; ... ]                     *)
    (* - Foo.( bar; ... )                     *)
    (* - method x : type a. ... = ...         *)
    (* - foo.@(bar)                           *)
    (* - foo.@(bar) <- baz                    *)
    (* - foo.%.{bar}                          *)
    (* - foo.%.{bar} <- baz                   *)
    (* - foo.%.[bar]                          *)
    (* - foo.%.[bar] <- baz                   *)
    (******************************************)

    method! expression_desc x acc =
      match x with
      | Pexp_record (labels, expr_o) ->
          let acc =
            self#list
              (fun (lid, e) acc ->
                if
                  Location.compare_pos lid.loc.loc_start e.pexp_loc.loc_start
                  = 0
                then
                  if Location.compare lid.loc e.pexp_loc = 0 then
                    (* punning. *)
                    self#longident_loc lid acc
                  else
                    match e.pexp_desc with
                    | Pexp_constraint (e, c) ->
                        (* { foo : int } and { foo : int = x } ... *)
                        let _ = self#core_type c acc in
                        self#expression e acc
                    | _ ->
                        (* No idea what's going on there. *)
                        self#expression e acc
                else
                  let acc = self#longident_loc lid acc in
                  let acc = self#expression e acc in
                  acc)
              labels acc
          in
          self#option self#expression expr_o acc
      | Pexp_open
          (({ popen_expr = { pmod_desc = Pmod_ident lid; _ }; _ } as opn), e)
        when Location.compare_pos lid.loc.loc_start e.pexp_loc.loc_start = 0
             && Location.compare_pos lid.loc.loc_end e.pexp_loc.loc_end <> 0 ->
          (* let's relocate ... *)
          let e_loc = { e.pexp_loc with loc_start = lid.loc.loc_end } in
          super#expression_desc
            (Pexp_open (opn, { e with pexp_loc = e_loc }))
            acc
      | Pexp_poly (e, Some { ptyp_desc = Ptyp_poly (_, ct); _ }) -> (
          match extract_constraint e with
          | Some (e, cte) when Location.compare cte.ptyp_loc ct.ptyp_loc = 0 ->
              let acc = self#expression e acc in
              let acc = self#core_type ct acc in
              acc
          | _ -> super#expression_desc x acc)
      | Pexp_apply ({ pexp_desc = Pexp_ident { txt = lid; _ }; _ }, args) -> (
          match Longident.last_exn lid with
          | id
            when String.is_prefix id ~prefix:"."
                 && (String.is_suffix id ~suffix:"()"
                    || String.is_suffix id ~suffix:"()<-"
                    || String.is_suffix id ~suffix:"[]"
                    || String.is_suffix id ~suffix:"[]<-"
                    || String.is_suffix id ~suffix:"{}"
                    || String.is_suffix id ~suffix:"{}<-") ->
              self#list (fun (_, e) -> self#expression e) args acc
          | exception _ -> super#expression_desc x acc
          | _ -> super#expression_desc x acc)
      | _ -> super#expression_desc x acc

    (*******************************************************)
    (* The following is overridden because of:             *)
    (* - punning.                                          *)
    (* - record field with type constraint.                *)
    (* - unpack locations being incorrect when constrained *)
    (*******************************************************)

    method! pattern_desc x acc =
      match x with
      | Ppat_record (labels, _) ->
          self#list
            (fun (lid, pat) acc ->
              if
                Location.compare_pos lid.loc.loc_start pat.ppat_loc.loc_start
                = 0
              then
                if Location.compare lid.loc pat.ppat_loc = 0 then
                  (* simple punning! *)
                  self#longident_loc lid acc
                else
                  match pat.ppat_desc with
                  | Ppat_constraint (p, c) ->
                      (* { foo : int } and { foo : int = x } ... *)
                      let _ = self#core_type c acc in
                      self#pattern p acc
                  | _ ->
                      (* No idea what's going on there. *)
                      self#pattern pat acc
              else
                let acc = self#longident_loc lid acc in
                let acc = self#pattern pat acc in
                acc)
            labels acc
      | Ppat_constraint ({ ppat_desc = Ppat_unpack a; _ }, b) ->
          let acc = self#loc (self#option self#string) a acc in
          self#core_type b acc
      | _ -> super#pattern_desc x acc

    (***********************************************************)
    (* The following is overridden because the location of the *)
    (* fake structure for a generative argument covers the     *)
    (* location of the functor.                                *)
    (***********************************************************)

    method! module_expr_desc x acc =
      match x with
      | Pmod_apply (m, { pmod_desc = Pmod_structure []; pmod_loc; _ })
        when Location.compare_pos m.pmod_loc.loc_start pmod_loc.loc_start = 0 ->
          super#module_expr m acc
      | _ -> super#module_expr_desc x acc

    (***********************************************************)
    (* The following is overridden because the location of the *)
    (* open_infos for Pcl_open only covers the "open" keyword  *)
    (* and not the module opened.                              *)
    (***********************************************************)

    method! class_expr_desc x acc =
      match x with
      | Pcl_open (od, ce) ->
          (* inline of open_description (which effectively makes that node
             disappear) *)
          let acc = self#longident_loc od.popen_expr acc in
          let acc = self#override_flag od.popen_override acc in
          let acc = self#location od.popen_loc acc in
          let acc = self#attributes od.popen_attributes acc in
          (* continue *)
          let acc = self#class_expr ce acc in
          acc
      | _ -> super#class_expr_desc x acc

    (*********************)
    (* Same as above ... *)
    (*********************)

    method! class_type_desc x acc =
      match x with
      | Pcty_open (od, ct) ->
          (* inline of open_description (which effectively makes that node
             disappear) *)
          let acc = self#longident_loc od.popen_expr acc in
          let acc = self#override_flag od.popen_override acc in
          let acc = self#location od.popen_loc acc in
          let acc = self#attributes od.popen_attributes acc in
          (* continue *)
          let acc = self#class_type ct acc in
          acc
      | _ -> super#class_type_desc x acc

    (**********************************************************)
    (* The following is overridden because docstrings have    *)
    (* the same location as the item they get attached to.    *)
    (**********************************************************)

    method! attribute x acc =
      match x.attr_name.txt with
      | "ocaml.doc" | "ocaml.text" -> acc
      | _ -> super#attribute x acc
  end
