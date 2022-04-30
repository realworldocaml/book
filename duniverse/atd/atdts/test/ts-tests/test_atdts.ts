// Test JSON reading and writing against expectations.

import * as API from "./everything"
import * as fs from "fs"

// Check that types are exported
const a: API.Int = 0
const b: API.Option<string> = null

function assert(is_true: boolean, errmsg: string) {
  if (!is_true) {
    throw new Error(errmsg)
  }
}

function save(file: string, data: string) {
  console.log('Saving data to file ' + file)
  fs.writeFile(file, data, err => {
    if (err)
      throw new Error(String(err))
  })
}

function test_everything() {
  const a_obj : API.Root = {
    id: "abc",
    this_: 100,
    items: [[], [1, 2]],
    // maybe?: 123,
    extras: [17, 53],
    answer: 42,
    aliased: [8, 9, 10],
    point: [3.3, -77.22],
    kinds: [
      { kind: 'WOW' },
      { kind: 'Thing', value: 99 },
      { kind: 'Amaze', value: ["a", "b"] },
      { kind: 'Root' }
    ],
    assoc1: [
      [1.1, 1],
      [2.2, 2],
    ],
    assoc2: [
      ["c", 3],
      ["d", 4],
    ],
    assoc3: new Map([
      [5.5, 5],
      [6.6, 6],
    ]),
    assoc4: new Map([
      ["g", 7],
      ["h", 8],
    ]),
    options: [{value:10}, null, {value:88}],
    nullables: [13, 71]
  }
  const a_str = JSON.stringify(API.writeRoot(a_obj), null, 2)
  save('a_str', a_str)

  console.log(
    `----- a_str (converted from original TS object) -----
${a_str}`
  )

  // expected output (copy-pasted from an earlier run)
  const b_str =
`{
  "ID": "abc",
  "this": 100,
  "items": [
    [],
    [
      1,
      2
    ]
  ],
  "extras": [
    17,
    53
  ],
  "answer": 42,
  "aliased": [
    8,
    9,
    10
  ],
  "point": [
    3.3,
    -77.22
  ],
  "kinds": [
    "wow",
    [
      "Thing",
      99
    ],
    [
      "!!!",
      [
        "a",
        "b"
      ]
    ],
    "Root"
  ],
  "assoc1": [
    [
      1.1,
      1
    ],
    [
      2.2,
      2
    ]
  ],
  "assoc2": {
    "c": 3,
    "d": 4
  },
  "assoc3": [
    [
      5.5,
      5
    ],
    [
      6.6,
      6
    ]
  ],
  "assoc4": {
    "g": 7,
    "h": 8
  },
  "options": [
    [
      "Some",
      10
    ],
    "None",
    [
      "Some",
      88
    ]
  ],
  "nullables": [
    13,
    71
  ]
}`
  save('b_str', b_str)
  const b_obj = API.readRoot(JSON.parse(a_str))
  const b_str2 = JSON.stringify(API.writeRoot(b_obj), null, 2)
  save('b_str2', b_str2)

  assert(
    b_str === b_str2,
    `JSON mismatch:
----- expected (b_str) -----
${b_str}
----- actual (b_str2) -----
${b_str2}
---------------------------`
  )
  assert(
    b_str2 === a_str,
    `JSON mismatch:
----- expected (b_str2) -----
${b_str2}
----- actual (a_str) -----
${a_str}
--------------------------`
  )
}

test_everything()
