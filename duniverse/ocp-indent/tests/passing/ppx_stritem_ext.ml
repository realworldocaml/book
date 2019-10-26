let x = 3

[%% a
  let x = [
    3;
    2;
  ]
]

module S = sig

  let x = 3

  [%% b
    let x = [
      3;
      2;
    ]
  ]

end

[%% c
  let x = [
    3;
    2;
  ]

  [%% d
    let x = [
      3;
      2;
    ]
  ]

]

[%% x
  2 * 3
  +
  x
]

[%% x
  2 + 3
      *
      x
]

[%% x
  2
]

[%% x
    .
    y
  2
]


[%%   x
      .y
  2
]

[%% x .
    y
  2
]

[%%
    x
  2
]

module S = sig

  let x = 3

  [%% x
      .y
    2
  ]

  [%%   x
        .y
    2
  ]

  [%%
      x
      .y
    2
  ]

end

[%% client

  open M
  let  x = 3
  module M = struct end

]

[%% client

  let  x = 3
  open M
  module M = struct end

]

[%% client

  module M = struct end
  open M
  let  x = 3
]

module M = struct
  type a = A of b [@@deriving compare]
  and b = B of a
end
