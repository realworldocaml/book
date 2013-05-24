# Parsing Binary Protocols with Bigarray

## Bigarrays for external memory blocks

An OCaml bigarray is a useful custom block provided as standard to manipulate
memory blocks outside the OCaml heap.  It has `Custom_tag` in the header, and
the first word points to the `custom_operations` struct for bigarrays.
Following this is a `caml_ba_array` struct.

```
struct caml_ba_array {
  void * data;                  /* Pointer to raw data */
  intnat num_dims;              /* Number of dimensions */
  intnat flags;                 /* Kind of element array + memory layout + allocation status */
  struct caml_ba_proxy * proxy; /* The proxy for sub-arrays, or NULL */
  intnat dim[]  /*[num_dims]*/; /* Size in each dimension */
};
```

The `data` is usually a pointer to a `malloc`'ed chunk of memory, which the
custom finalizer operation `free`'s when the block is free.  The `flags` field
encodes three values, located in the bits as specified by three masks:

```
CAML_BA_KIND_MASK = 0xFF     /* Mask for kind in flags field */
CAML_BA_LAYOUT_MASK = 0x100  /* Mask for layout in flags field */
CAML_BA_MANAGED_MASK = 0x600 /* Mask for "managed" bits in flags field */
```

The `CAML_BA_KIND_MASK` bits hold a value of the `caml_ba_kind` enum that identifies the
kind of value in the bigarray `data`.

```
enum caml_ba_kind {
  CAML_BA_FLOAT32,             /* Single-precision floats */
  CAML_BA_FLOAT64,             /* Double-precision floats */
  CAML_BA_SINT8,               /* Signed 8-bit integers */
  CAML_BA_UINT8,               /* Unsigned 8-bit integers */
  CAML_BA_SINT16,              /* Signed 16-bit integers */
  CAML_BA_UINT16,              /* Unsigned 16-bit integers */
  CAML_BA_INT32,               /* Signed 32-bit integers */
  CAML_BA_INT64,               /* Signed 64-bit integers */
  CAML_BA_CAML_INT,            /* OCaml-style integers (signed 31 or 63 bits) */
  CAML_BA_NATIVE_INT,          /* Platform-native long integers (32 or 64 bits) */
  CAML_BA_COMPLEX32,           /* Single-precision complex */
  CAML_BA_COMPLEX64,           /* Double-precision complex */
}
```

The `CAML_BA_LAYOUT_MASK` bit says whether multi-dimensional arrays are layed out C or
Fortran style.

```
enum caml_ba_layout {
  CAML_BA_C_LAYOUT = 0,           /* Row major, indices start at 0 */
  CAML_BA_FORTRAN_LAYOUT = 0x100, /* Column major, indices start at 1 */
};
```

The `CAML_BA_MANAGED_MASK` bits hold a value of the `caml_ba_managed` enum that identifies
whether OCaml is responsible for freeing the `data` or some other code is.

```
enum caml_ba_managed {
  CAML_BA_EXTERNAL = 0,        /* Data is not allocated by OCaml */
  CAML_BA_MANAGED = 0x200,     /* Data is allocated by OCaml */
  CAML_BA_MAPPED_FILE = 0x400, /* Data is a memory mapped file */
};
```

