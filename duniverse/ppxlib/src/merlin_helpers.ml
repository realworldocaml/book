open! Import

let mknoloc txt = Location.{ txt; loc = none }

let hide_attribute  : attribute = mknoloc "merlin.hide",  PStr []
let focus_attribute : attribute = mknoloc "merlin.focus", PStr []

let hide_pattern ({ ppat_attributes ; _ } as p) =
  { p with ppat_attributes = hide_attribute :: ppat_attributes }
let focus_pattern ({ ppat_attributes ; _ } as p) =
  { p with ppat_attributes = focus_attribute :: ppat_attributes }

let hide_expression ({ pexp_attributes ; _ } as e) =
  { e with pexp_attributes = hide_attribute :: pexp_attributes }
let focus_expression ({ pexp_attributes ; _ } as e) =
  { e with pexp_attributes = focus_attribute :: pexp_attributes }
