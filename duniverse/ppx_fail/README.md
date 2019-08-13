ppx_fail
========

Syntax extension that makes [failwiths] always include a position.

Rewrites `failwiths` into `failwiths ~here:[%here]`. This is useful to have a position
for an exception even if the backtrace got lost somehow.
