## Examples

### cloc

This program reads C-sources from stdin and prints them to stdout with
comments and empty lines removed.  Useful for counting LOCs.

### count_hash

This program reads text from stdin, counts all equal words that are separated by
whitespace and prints the result to stdout.

### pcregrep

A grep-like program using Perl-compatible regular expressions.  Start the
program with argument `-help` to see what it does!

### subst

Substitutes text in files using Perl-compatible regular expressions and
substitution patterns.  Start the program with argument `-help` to see what it
does!

Example invocation:

```sh
subst '([Tt])ermcap' '$1ermCap' < /etc/termcap
```

### dfa_restart

Exercises the availability of the DFA matching function and its partial
match restart capability.  Given a pattern, will accept input incrementally,
restarting the prior partial match until the pattern succeeds in matching
completely, or fails.

Example interaction:

```
$ dfa_restart.exe 'abc12+3'
> abc
partial match, provide more input:
> 122222
partial match, provide more input:
> 222
partial match, provide more input:
> 3
match completed: "[|0;1;0|]"
```
