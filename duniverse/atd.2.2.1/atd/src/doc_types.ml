type inline =
  | Text of string
  | Code of string

type block =
  | Paragraph of inline list
  | Pre of string
