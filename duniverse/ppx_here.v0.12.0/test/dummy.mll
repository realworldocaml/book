{
  let _ = [%here]
}

rule a = parse
| _ { ignore ([%here]); assert false }
