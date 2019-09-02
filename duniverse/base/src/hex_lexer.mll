{
type result =
| Neg of string
| Pos of string
}

let hex_digit = ['0' - '9' 'A' - 'F' 'a' - 'f']
let body = (hex_digit (hex_digit | '_')*) as body
let body_with_suffix = '0' ['X' 'x'] body
let pos = body_with_suffix
let neg = '-' body_with_suffix

rule parse_hex = parse
| neg                     { Neg body }
| pos                     { Pos body }
