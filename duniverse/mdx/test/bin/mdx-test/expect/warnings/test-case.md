No warning is printed by default:

```ocaml
type p = { x : int ; y : int }

let x { x } = x
```

Warning attributes must be set to print them:

```ocaml version<4.12
[@@@warning "+9"]
let x { x } = x
```
```mdx-error
...
Warning 9: the following labels are not bound in this record pattern:
y
Either bind these labels explicitly or add '; _' to the pattern.
```

```ocaml version>=4.12
[@@@warning "+9"]
let x { x } = x
```
```mdx-error
Line 2, characters 9-14:
Warning 9 [missing-record-field-pattern]: the following labels are not bound in this record pattern:
y
Either bind these labels explicitly or add '; _' to the pattern.
```
