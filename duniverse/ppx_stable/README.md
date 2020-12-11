ppx\_stable
===========

A ppx extension for easier implementation of conversion functions between almost
identical types.

Overview
--------

It's very common in the stable types idiom to have types like these:

```ocaml
module Stable = struct
  module V1 = struct
    type t =
      { x0 : X0.t
      ; x1 : X1.t
      ; ...
      ; xn : Xn.t
      }
    end
  end

  module V2 = struct
    type t =
      { x0 : X0.t
      ; x1 : X1.t
      ; ...
      ; x(n+1) : X(n+1).t
      }
    end
  end
end
```

Implementing a conversion function is quite easy but the length of the
implementation is linear to the size of record instead of linear to the number
of differences between the records, i.e.

```ocaml
let v1_of_v2 { V2. x0 ; x1 ; ... ; xn ; x(n+1) } =
  ignore (x(n+1) : X(n+1).t); (* forget the new value *)
  { V1. x0 ; x1 ; ...  ; xn }
```

If there are more changes to the type the conversion function get unnecessarily
complicated.

This extension proposes a syntax that essentially describes the differences
between the types:

```ocaml
module V2 = struct
  type t =
    { x0 : X0.t
    ; x1 : X1.t
    ; ...
    ; x(n+1) : X(n+1).t
    } [@@deriving stable_record ~version:V1.t ~remove:[x(n+1)]]
  end
end
```

and automatically generates the appropriate conversion function.

Details
------

It supports records with conversion from the _current_ type to the previous
and from previous to _current_. It works for variants and records.

## Records

It supports all the possible ways you can change a record:
- adding new fields
- modifying existing fields, i.e. changing its type
- removing some fields

### Adding a field

```ocaml
module V1 = struct
  type t =
    { x0 : X0.t
    ; x1 : X1.t
    }
end

module V2 = struct
  type t =
    { x0 : X0.t
    } [@@deriving stable_record ~version:V1.t ~add:[x1]]
end

let convert_to_v1 (v2 : V2.t) : V1.t =
  V2.to_V1_t v2 ~x1:(X1.of_int 1234)
```

### Modifying a field

```ocaml
module V1 = struct
  type t =
    { x0 : X0a.t
    }
end

module V2 = struct
  type t =
    { x0 : X0b.t
    } [@@deriving stable_record ~version:V1.t ~modify:[x0]]
end

let convert_of_v1 (v1 : V1.t) : V2.t =
  V2.of_V1_t v1 ~modify_x0:X0b.of_x0a
```

or

```ocaml
module V1 = struct
  type t =
    { x0 : X0a.t
    }
end

module V2 = struct
  type t =
    { x0 : X0b.t
    } [@@deriving stable_record ~version:V1.t ~set:[x0]]
end

let convert_of_v1 (v1 : V1.t) : V2.t =
  V2.of_V1_t v1 ~x0:X0b.value
```

### Removing a field

```ocaml
module V1 = struct
  type t =
    { x0 : X0.t
    }
end

module V2 = struct
  type t =
    { x0 : X0.t
    ; x1 : X1.t
    } [@@deriving stable_record ~version:V1.t ~remove:[x1]]
end

let convert_to_v1 (v2 : V2.t) : V1.t =
  V2.to_V1_t v2
```

## Variants

Please note that for variants you have to derive `stable_variant` for the target type as well.

### Adding a constructor

```ocaml
module V1 = struct
  type t =
    | X0 of X0.t
    | X1 of X1.t
  [@@deriving stable_variant]
end

module V2 = struct
  type t =
    | X0 of X0.t
    [@@deriving stable_variant ~version:V1.t ~add:[X1]]
end

let convert_to_v1 (v2 : V2.t) : V1.t =
  V2.to_V1_t v2
```

### Modifying a constructor

```ocaml
module V1 = struct
  type t =
    | X0 of X0a.t
  [@@deriving stable_variant]
end

module V2 = struct
  type t =
    | X0 of X0b.t
    [@@deriving stable_variant ~version:V1.t ~modify:[X0]]
end

let convert_of_v1 (v1 : V1.t) : V2.t =
  V2.of_V1_t ~modify_X0:(fun v -> X0 (X0b.of_x0a v)) v1
```

### Removing a constructor

```ocaml
module V1 = struct
  type t =
    | X0 of X0.t
  [@@deriving stable_variant]
end

module V2 = struct
  type t =
    | X0 of X0.t
    | X1 of X1.t
    [@@deriving stable_variant ~version:V1.t ~remove:[X1]]
end

let convert_to_v1 (v2 : V2.t) : V1.t =
  V2.to_V1_t ~remove_X1:(fun v -> X0 (X0b.of_int v)) v2
```

### Renaming a constructor

```ocaml
module V1 = struct
  type t =
    | Do
    | Re
    | Mi of int
  [@@deriving stable_variant]
end

module V2 = struct
  type t =
    | Do
    | Re
    | Ew of int
  [@@deriving stable_variant ~version:V1.t ~remove:[ Ew ] ~add:[ Mi ]]
end

let v2_of_v1 : V1.t -> V2.t = V2.of_V1_t ~remove_Mi:(fun i -> V2.Ew i)
```
