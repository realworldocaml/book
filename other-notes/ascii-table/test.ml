open Core.Std

let print_table header rows =
  printf "%s\n\n%!" (Ascii_table.render_table header rows)

let () =
  print_table
    ["Name";"Age";"Description"]
    [ ["Yaron Minsky";"39";"Serial entrepeneur"]
    ; ["Anil Madhavapeddy";"37";"Gawky uninteresting geektron"]
    ]


type entry =
  { name: string;
    age: int;
    description: string;
  }

let col = Ascii_table.column
let name_col    = col "Name" (fun t -> t.name)
let age         = col "Age" (fun t -> Int.to_string t.age)
let description = col "Description" (fun t -> t.description)

let people = [
  { name = "Yaron Minsky"; age=39; description="Serial entrepeneur" };
  { name = "Anil Madhavapeddy"; age=39; description="Geeky geektron" };
  { name = "Nava Yaffa"; age=39; description="Siggy" };
]


let () =
  printf "%s\n%!" (Ascii_table.render_columns
                     [name_col;age;description;age] people)
