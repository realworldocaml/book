1.
  The purpose of the code is to allow the functional update
  `{< vertices = Array.map matrix#transform vertices >`};
  for this to work, `vertices` must be a field of the object.
  If the field definition were omitted, the functional update would fail.

