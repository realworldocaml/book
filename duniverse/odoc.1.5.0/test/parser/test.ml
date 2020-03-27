type test_case = {
  name : string;
  parser_input : string;
  sections_allowed : [ `All | `No_titles | `None ];
  location : Odoc_model.Location_.point;
}

let make_test_case
    ?(sections_allowed = `No_titles)
    ?(location = {Odoc_model.Location_.line = 1; column = 0})
    name
    parser_input =
  {name; parser_input; sections_allowed; location}

let t = make_test_case

type test_suite = string * (test_case list)



let tests : test_suite list = [
  "trivial", [
    t "empty" "";
    t "space" " ";
    t "two-spaces" "  ";
    t "tab" "\t";
    t "mixed-space" " \t \t";
    t "newline" "\n";
    t "blank-line" "\n\n";
    t "cr-lf" "\r\n";
  ];

  "one-paragraph", [
    t "word" "foo";
    t "two-words" "foo bar";
    t "two-spaces" "foo  bar";
    t "mixed-space" "foo \t \t bar";
    t "two-lines" "foo\nbar";
    t "two-lines-cr-lf" "foo\r\nbar";
    t "leading-space" " foo";
    t "trailing-space" "foo ";
    t "leading-space-on-line" "foo\n bar";
    t "trailing-space-on-line" "foo \nbar";
    t "leading-tab-on-line" "foo\n\tbar";
    t "trailing-tab-on-line" "foo\t\nbar";
    t "email" "foo@bar.com";
  ];

  "two-paragraphs", [
    t "basic" "foo\n\nbar";
    t "trailing-space" "foo \n\nbar";
    t "leading-space" "foo\n\n bar";
    t "cr-lf" "foo\r\n\r\nbar";
    t "mixed-cr-lf" "foo\n\r\nbar";
  ];

  "plus-minus-words", [
    t "minus-in-word" "foo-bar";
    t "minus-as-word" "foo -";
    t "plus-in-word" "foo+bar";
    t "plus-as-word" "foo +";
    t "negative-number" "-3.14 -1337";
    t "en-em-dash" "-- ---";
    t "minus-at" "-@";
    t "at-minus" "-@-";
    t "option" "--option";
  ];

  "escape-sequence", [
    t "left-brace" "\\{";
    t "left-brace-in-word" "foo\\{bar";
    t "right-brace" "\\}";
    t "right-brace-in-word" "foo\\}bar";
    t "left-bracket" "\\[";
    t "left-bracket-in-word" "foo\\[bar";
    t "right-bracket" "\\]";
    t "right-bracket-in-word" "foo\\]bar";
    t "at" "\\@";
    t "not-a-tag" "\\@author";
    t "at-in-word" "foo\\@bar";
    t "trailing-backslash" "foo\\";
    t "non-escape" "foo\\bar";
    t "backslash-not-escaped" "foo\\\\{bar";
    t "single-backslash" "\\";
    t "escape-minus" "\\{- foo";
    t "escape-plus" "\\{+ foo";
    t "minus-escape" "-\\{";
    t "plus-escape" "+\\{";
    t "escape-at" "\\{@author";
    t "two" "\\{\\}";
  ];

  "code-span", [
    t "basic" "[foo]";
    t "empty" "[]";
    t "list" "[[]]";
    (* TODO The next two error messages are particularly unintuitive. *)
    t "unbalanced-list" "[[]";
    t "no-markup" "[{b]";
    t "few-escapes" "[\\{]";
    t "escaped-right-bracket" "[\\]]";
    t "escaped-left-bracket" "[\\[]";
    t "whitespace-preserved" "[ foo  bar ]";
    t "no-newlines" "[foo\nbar]";
    t "cr-lf-preserved" "[foo\r\nbar]";
    t "no-double-newline" "[foo\n\nbar]";
    t "no-double-crlf" "[foo\r\n\r\nbar]";
    t "not-merged" "[foo][bar]";
    t "explicit-space" "[foo] [bar]";
    t "unterminated" "[foo";
  ];

  "bold", [
    t "basic" "{b foo}";
    t "extra-leading-whitespace" "{b  \t foo}";
    t "leading-newline" "{b\nfoo}";
    t "leading-cr-lf" "{b\r\nfoo}";
    t "leading-newline-and-whitespace" "{b\n foo}";
    t "no-leading-whitespace" "{bfoo}";
    t "trailing-whitespace" "{b foo }";
    t "trailing-newline" "{b foo\n}";
    t "trailing-cr-lf" "{b foo\r\n}";
    t "two-words" "{b foo bar}";
    t "not-merged" "{b foo}{b bar}";
    t "nested" "{b foo{b bar}}";
    t "newline" "{b foo\nbar}";
    t "cr-lf" "{b foo\r\nbar}";
    t "minus" "{b -}";
    t "minus-list-item" "{b foo\n - bar}";
    t "plus-list-item" "{b foo\n + bar}";
    t "immediate-minus-list-item" "{b\n- foo}";
    t "immediate-plus-list-item" "{b\n+ foo}";
    t "blank-line" "{b foo\n\nbar}";
    t "immediate-blank-line" "{b\n\n";
    t "end-of-comment" "{b foo";
    t "nested-code-block" "{b {[foo]}";
    t "degenerate" "{b}";
    t "empty" "{b }";
  ];

  "italic", [
    t "basic" "{i foo}";
    t "extra-leading-whitespace" "{i  \t foo}";
    t "leading-newline" "{i\nfoo}";
    t "leading-newline-and-whitespace" "{i\n foo}";
  ];

  "emphasis", [
    t "basic" "{e foo}";
    t "extra-leading-whitespace" "{e  \t foo}";
    t "leading-newline" "{e\nfoo}";
    t "leading-newline-and-whitespace" "{e\n foo}";
  ];

  "superscript", [
    t "basic" "{^ foo}";
    t "extra-leading-whitespace" "{^  \t foo}";
    t "leading-newline" "{^\nfoo}";
    t "leading-cr-lf" "{^\r\nfoo}";
    t "leading-newline-and-whitespace" "{^\n foo}";
    t "no-whitespace" "{^foo}";
    t "degenerate" "{^}";
    t "empty" "{^ }";
  ];

  "subscript", [
    t "basic" "{_ foo}";
    t "extra-leading-whitespace" "{_  \t foo}";
    t "leading-newline" "{_\nfoo}";
    t "leading-newline-and-whitespace" "{_\n foo}";
    t "no-whitespace" "{_foo}";
    t "v-verbose" "{_uv}";
  ];

  "simple-reference", [
    t "basic" "{!foo}";
    t "leading-whitespace" "{! foo}";
    t "trailing-whitespace" "{!foo }";
    t "adjacent-word-leading" "bar{!foo}";
    t "explicit-leading-space" "bar {!foo}";
    t "adjacent-word-trailing" "{!foo}bar";
    t "explicit-trailing-space" "{!foo} bar";
    t "kind" "{!val:foo}";
    t "empty" "{!}";
    t "whitespace-only" "{! }";
    t "internal-whitespace" "{!( * )}";
    (* TODO Limiting the character combinations allowed will make it easier to
       catch expressions accidentally written inside references. This can also
       be caught by a good resolver and resolver error messages. *)
    (* t "expression" *)
    t "unterminated" "{!foo";
    t "empty-kind" "{!:foo}";
    t "whitespace-kind" "{! :foo}";
    t "with-kind-but-empty" "{!val:}";
    t "with-kind-but-whitespace" "{!val: }";
    t "leading-whitespace-in-kind" "{! val:foo}";
    t "internal-whitespace-in-kind" "{!va l:foo}";
    t "internal-whitespace-in-referent" "{!val:( * )}";
    t "two-colons" "{!val:foo:bar}";
    t "space-before-colon" "{!val :foo}";
    t "space-after-colon" "{!val: foo}";
    t "unterminated-after-kind" "{!val:foo";
    t "operator" "{!(>>=)}";
    t "operator-with-dash" "{!(@->)}";
    t "operator-with-dot" "{!(*.)}";
    t "operator-with-colon" "{!(>::)}";
  ];

  "reference-with-text", [
    t "basic" "{{!foo} bar}";
    t "degenerate" "{{!foo}}";
    t "empty" "{{!foo} }";
    t "nested-markup" "{{!foo} {b bar}}";
    t "in-markup" "{e {{!foo} bar}}";
    t "no-separating-space" "{{!foo}bar}";
    t "kind" "{{!val:foo} bar}";
    t "nested-reference" "{{!foo} {!bar}}";
    t "nested-empty" "{{!foo} {{!bar}}}";
    t "nested-through-emphasis" "{{!foo} {e {{!bar} baz}}}";
    t "simple-through-emphasis" "{{!foo} {e {!bar}}}";
    t "empty-target" "{{!} foo}";
    t "whitespace-only-in-target" "{{! } foo}";
    t "internal-whitespace" "{{!( * )} baz}";
    t "unterminated" "{{!foo";
    t "unterminated-content" "{{!foo} bar";
  ];

  "link", [
    t "basic" "{{:foo} bar}";
    t "nested-markup" "{{:foo} {b bar}}";
    t "in-markup" "{e {{:foo} bar}}";
    t "no-separating-space" "{{:foo}bar}";
    t "nested-link" "{{:foo} {{:bar} baz}}";
    t "nested-through-emphasis" "{{:foo} {e {{:bar} baz}}}";
    t "reference-through-emphasis" "{{:foo} {e {!bar}}}";
    t "nested-in-reference" "{{!foo} {e {{:bar} baz}}}";
    t "empty-target" "{{:} foo}";
    t "whitespace-only-in-target" "{{: } foo}";
    t "empty" "{{:foo}}";
    t "internal-whitespace" "{{:foo bar} baz}";
    t "unterminated" "{{:foo";
    t "single-braces" "{:foo}";
    t "unterminated-single-braces" "{:foo";
    t "empty-single-braces" "{:}";
    t "single-braces-whitespace-only" "{: }";
  ];

  "raw-markup", [
    t "html-target" "{%html:foo%}";
    t "whitespace" "{%html: foo bar %}";
    t "whitespace-only" "{%html: %}";
    t "empty" "{%html:%}";
    t "html-payload" "{%html:<e>foo</e>%}";
    t "colon" "{%html:foo:bar%}";
    t "no-target" "{%foo%}";
    t "empty-target" "{%:foo%}";
    t "whitespace-target" "{% :foo%}";
    t "invalid-target" "{%xml:foo%}";
    t "incorrect-case-target" "{%HTML:foo%}";
    t "multiline-target" "{%\n:foo%}";
    t "percent-in-target" "{%%:%}";
    t "percent-in-payload" "{%html:%%}";
    t "multiple-percent-in-target" "{%%%foo%%:%}";
    t "multiple-percent-in-payload" "{%html:%%foo%%%}";
    t "opener-in-target" "{%{%:foo%}";
    t "opener-in-payload" "{%html:{%%}";
    t "right-brace-in-target" "{%}:%}";
    t "right-brace-in-payload" "{%html:}%}";
    t "unterminated" "{%";
    t "unterminated-after-target" "{%html:";
    t "degenerate" "{%}";
  ];

  "module-list", [
    t "basic" "{!modules:Foo}";
    t "two" "{!modules:Foo Bar}";
    t "extra-whitespace" "{!modules: Foo  Bar }";
    t "newline" "{!modules:Foo\nBar}";
    t "cr-lf" "{!modules:Foo\r\nBar}";
    t "empty" "{!modules:}";
    t "whitespace-only" "{!modules: }";
    t "unterminated" "{!modules:";
    t "in-paragraph" "foo {!modules:Foo}";
    t "followed-by-word" "{!modules:Foo} foo";
    t "in-list" "- {!modules:Foo}";
  ];

  "code-block", [
    t "basic" "{[foo]}";
    t "empty" "{[]}";
    t "whitespace-only" "{[ ]}";
    t "blank-line-only" "{[\n  \n]}";
    t "whitespace" "{[foo bar]}";
    t "newline" "{[foo\nbar]}";
    t "cr-lf" "{[foo\r\nbar]}";
    t "blank-line" "{[foo\n\nbar]}";
    t "leading-whitespace" "{[ foo]}";
    t "leading-whitespace-two" "{[ foo\n bar]}";
    t "leading-whitespace-two-cr-lf" "{[ foo\r\n bar]}";
    t "leading-whitespace-two-different-indent" "{[ foo\n   bar]}";
    t "leading-whitespace-two-different-indent-rev" "{[   foo\n bar]}";
    t ~location:{Odoc_model.Location_.line = 1; column = 3}
      "leading-whitespace-two-different-indent-reloc" "{[ foo\n      bar]}";
    t "leading-whitespace-with-empty-line" "{[ foo\n\n bar]}";
    t "leading-whitespace-with-whitespace-line-short" "{[  foo\n \n  bar]}";
    t "leading-whitespace-with-whitespace-line-long" "{[ foo\n   \n bar]}";
    t "leading-whitespace-leading-newline" "{[\n  foo\n  bar\n]}";
    t "leading-tab" "{[\tfoo]}";
    t "leading-tab-two" "{[\tfoo\n\tbar]}";
    t "leading-tab-two-different-indent" "{[\tfoo\n\t\tbar]}";
    t "leading-newline" "{[\nfoo]}";
    t "leading-cr-lf" "{[\r\nfoo]}";
    t "leading-newlines" "{[\n\nfoo]}";
    t "leading-newline-with-space" "{[\n foo]}";
    t "leading-newline-with-trash" "{[ \nfoo]}";
    t "nested-opener" "{[{[]}";
    t "nested-closer" "{[foo]}]}";
    t "nested-bracket" "{[]]}";
    t "two-nested-brackets" "{[]]]}";
    t "nested-brackets-in-text" "{[foo]]bar]}";
    t "trailing-whitespace" "{[foo ]}";
    t "trailing-tab" "{[foo\t]}";
    t "trailing-newline" "{[foo\n]}";
    t "trailing-cr-lf" "{[foo\r\n]}";
    t "trailing-newlines" "{[foo\n\n]}";
    t "preceded-by-whitespace" " {[foo]}";
    t "followed-by-whitespace" "{[foo]} ";
    t "two-on-one-line" "{[foo]} {[bar]}";
    t "two" "{[foo]}\n{[bar]}";
    t "two-with-blank-line" "{[foo]}\n\n{[bar]}";
    t "followed-by-words" "{[foo]} bar";
    t "preceded-by-words" "foo {[bar]}";
    t "preceded-by-paragraph" "foo\n{[bar]}";
    t "followed-by-paragraph" "{[foo]}\nbar";
    t "unterminated" "{[foo";
    t "unterminated-bracket" "{[foo]";
    t "trailing-cr" "{[foo\r]}";
    t "comment" "{[(* foo *)\nlet bar = ()]}";
    t "docstring" "{[(** foo *)\nlet bar = ()]}";
    t "docstring-with-code-block" "{[(** {[foo]} *)\nlet bar = ()]}";
  ];

  "verbatim", [
    t "basic" "{v foo v}";
    t "empty" "{v v}";
    t "degenerate" "{vv}";
    t "whitespace-only" "{v  v}";
    t "blank-line-only" "{v\n  \nv}";
    t "no-leading-whitespace" "{vfoo v}";
    t "no-trailing-whitespace" "{v foov}";
    t "multiple-leading-whitespace" "{v  foo v}";
    t "multiple-trailing-whitespace" "{v foo  v}";
    t "leading-tab" "{v\tfoo v}";
    t "leading-newline" "{v\nfoo v}";
    t "leading-cr-lf" "{v\r\nfoo v}";
    t "trailing-tab" "{v foo\tv}";
    t "trailing-newline" "{v foo\nv}";
    t "trailing-cr-lf" "{v foo\r\nv}";
    t "internal-whitespace" "{v foo bar v}";
    t "newline" "{v foo\nbar v}";
    t "cr-lf" "{v foo\r\nbar v}";
    t "blank-line" "{v foo\n\nbar v}";
    t "leading-newlines" "{v\n\nfoo v}";
    t "leading-newline-with-space" "{v\n foo v}";
    t "leading-newline-with-trash" "{v \nfoo v}";
    t "nested-opener" "{v {v v}";
    t "nested-closer" "{v foo v} v}";
    t "nested-closer-with-word" "{v {dev} v}";
    t "nested-v" "{v v v}";
    t "two-nested-vs" "{v vv v}";
    t "nested-v-at-end" "{v vv}";
    t "two-nested-vs-at-end" "{v vvv}";
    t "nested-vs-in-text" "{v foovvbar v}";
    t "trailing-newlines" "{v foo\n\nv}";
    t "preceded-by-whitespace" " {v foo v}";
    t "followed-by-whitespace" "{v foo v} ";
    t "two-on-one-line" "{v foo v} {v bar v}";
    t "two" "{v foo v}\n{v bar v}";
    t "two-with-blank-line" "{v foo v}\n\n{v bar v}";
    t "followed-by-words" "{v foo v} bar";
    t "preceded-by-words" "foo {v bar v}";
    t "preceded-by-paragraph" "foo\n{v bar v}";
    t "followed-by-paragraph" "{v foo v}\nbar";
    t "unterminated" "{v foo";
    t "unterminated-v" "{v foo v";
    t "trailing-cr" "{v foo\rv}";
  ];

  "shorthand-list", [
    t "basic" "- foo";
    t "multiple-items" "- foo\n- bar";
    t "two-lists" "- foo\n\n- bar";
    t "ordered" "+ foo";
    t "leading-whitespace" " - foo";
    t "trailing-whitespace" "- foo ";
    t "bullet-in-line" "- foo - bar";
    t "bullet-in-line-immediately" "- - foo";
    t "code-block" "- {[foo]}";
    t "verbatim" "- {v foo v}";
    t "multiple-blocks" "- foo\n{[bar]}";
    t "followed-by-code-block" "- foo\n\n{[bar]}";
    t "different-kinds" "- foo\n+ bar";
    t "no-content" "-";
    t "immediate-newline" "-\nfoo";
    t "immediate-blank-line" "-\n\nfoo";
    t "immediate-markup" "-{b foo}";
    t "after-code-block" "{[foo]} - bar";
  ];

  "explicit-list", [
    t "basic" "{ul {li foo}}";
    t "ordered" "{ol {li foo}}";
    t "two-items" "{ul {li foo} {li bar}}";
    t "items-on-separate-lines" "{ul {li foo}\n{li bar}}";
    t "blank-line" "{ul {li foo}\n\n{li bar}}";
    t "blank-line-in-item" "{ul {li foo\n\nbar}}";
    t "junk" "{ul foo}";
    t "junk-with-no-whitespace" "{ulfoo}";
    t "empty" "{ul}";
    t "unterminated-list" "{ul";
    t "no-whitespace" "{ul{li foo}}";
    t "whitespace-at-end-of-item" "{ul {li foo\n\n\n}}";
    t "unterminated-{li" "{ul {li foo";
    t "unterminated-{-" "{ul {- foo";
    t "empty-{li" "{ul {li }}";
    t "empty-{-" "{ul {- }}";
    t "{li-without-whitespace" "{ul {lifoo}}";
    t "{li-followed-by-newline" "{ul {li\nfoo}}";
    t "{li-followed-by-cr-lf" "{ul {li\r\nfoo}}";
    t "{li-followed-by-blank-line" "{ul {li\n\nfoo}}";
    t "{--without-whitespace" "{ul {-foo}}";
    t "mixed-list-items" "{ul {li foo} {- bar}}";
    t "nested" "{ul {li {ul {li foo}}}}";
    t "shorthand-in-explicit" "{ul {li - foo\n- bar}}";
    t "explicit-in-shorthand" "- {ul {li foo}}";
    t "bare-{li" "{li foo}";
    t "bare-{-" "{- foo";
    t "after-code-block" "{[foo]} {ul {li bar}}";
  ];

  "heading", [
    t "basic" "{2 Foo}";
    t "subsection" "{3 Foo}";
    t "subsubsection" "{4 Foo}";
    t "leading-whitespace" "{2  Foo}";
    t "no-leading-whitespace" "{2Foo}";
    t "no-leading-whitespace-h3" "{3Foo}";
    t "leading-newline" "{2\nFoo}";
    t "leading-cr-lf" "{2\r\nFoo}";
    t "leading-blank-line" "{2\n\nFoo}";
    t "leading-blank-line-h3" "{3\n\nFoo}";
    t "trailing-whitespace" "{2 Foo }";
    t "trailing-newline" "{2 Foo\n}";
    t "trailing-blank-line" "{2 Foo\n\n}";
    t "nested-markup" "{2 [foo]}";
    t "nested-code-with-uppercase" "{2 [Foo]}";
    t "nested-code-with-spaces" "{2 [ foo bar  baz  \t]}";
    t "nested-code-with-newline" "{2 [foo\nbar\r\nbaz]}";
    t "nested-style" "{2 {e foo bar}}";
    t "words" "{2 foo bar}";
    t "nested-heading" "{2 {2 Foo}}";
    t "in-list" "- {2 Foo}";
    t "followed-by-junk" "{2 Foo} bar";
    t "preceded-by-junk" "foo {2 Bar}";
    t "followed-by-block" "{2 Foo}\nbar";
    t "preceded-by-block" "foo\n{2 Bar}";
    t "label" "{2:foo Bar}";
    t "whitespace-before-colon" "{2 :foo Bar}";
    t "whitespace-after-colon" "{2: foo Bar}";
    t "label-only" "{2:foo}";
    t "label-only-with-whitespace" "{2:foo }";
    t "in-list-outside-item" "{ul {2 Foo}}";
    t "preceded-by-shorthand-list" "- foo\n{2 Bar}";
    t "nested-in-two-lists" "{ul {li - foo\n{2 Bar}}}";
    t "bad-level-long-number" "{22 Foo}";
    t "bad-level-long-number-with-label" "{22:foo Bar}";
    t "bad-level-leading-zero" "{02 Foo}";
    t "bad-level-leading-zero-with-label" "{02:foo Bar}";
    t "bad-level-title" "{0 Foo}";
    t "bad-level-too-deep" "{6 Foo}";
    t "link-in-markup" "{2 {{:foo}}}";
    t "reference-in-markup" "{2 {!foo}}";
    t "two" "{2 Foo}\n{2 Bar}";
    t "greater" "{2 Foo}\n{3 Bar}";
  ];

  "section-contexts", [
    t "titles-allowed" "{0 Foo}"
      ~sections_allowed:`All;
    t "titles-no-high-levels" "{6 Foo}"
      ~sections_allowed:`All;
    t "two-titles" "{0 Foo}\n{0 Bar}"
      ~sections_allowed:`All;
    t "no-heading" "foo"
      ~sections_allowed:`All;
    t "heading-after-paragraph" "foo\n{0 Bar}"
      ~sections_allowed:`All;
    t "two-top-level-section-headings" "{1 Foo}\n{1 Bar}"
      ~sections_allowed:`All;
    t "two-headings-second-higher" "{1 Foo}\n{0 Bar}"
      ~sections_allowed:`All;
    t "three-headings-last-two-higher" "{3 Foo}\n{1 Bar}\n{2 Baz}"
      ~sections_allowed:`All;
    t "none" "{1 Foo}"
      ~sections_allowed:`None;
    t "title-no-titles-allowed" "{0 Foo}"
      ~sections_allowed:`No_titles;
    t "two-titles-none-allowed" "{0 Foo}\n{0 Bar}"
      ~sections_allowed:`No_titles;
    t "two-headings-none-allowed" "{1 Foo}\n{1 Bar}"
      ~sections_allowed:`None;
    t "multiple-with-bad-section" "{0 Foo}\n{0 Foo}\n{6 Foo}"
      ~sections_allowed:`All;
    t "promoted-duplicates" "{6 Foo}\n{6 Bar}"
      ~sections_allowed:`All;
    t "section-promoted-to-duplicate" "{5 Foo}\n{6 Bar}"
      ~sections_allowed:`All;
  ];

  "author", [
    t "basic" "@author Foo Bar";
    t "empty" "@author";
    t "whitespace-only" "@author ";
    t "extra-whitespace" "@author  Foo Bar ";
    t "newline" "@author Foo Bar\n";
    t "cr-lf" "@author Foo Bar\r\n";
    t "blank-line" "@author Foo Bar\n\n";
    t "followed-by-junk" "@author Foo\nbar";
    t "followed-by-code-span" "@author Foo\n[bar]";
    t "followed-by-code-block" "@author Foo\n{[bar]}";
    t "followed-by-verbatim" "@author Foo\n{v bar v}";
    t "followed-by-modules" "@author foo\n{!modules:Foo}";
    t "followed-by-list" "@author Foo\n{ul {li bar}}";
    t "followed-by-shorthand-list" "@author Foo\n- bar";
    t "followed-by-section-heading" "@author Foo\n{2 Bar}";
    t "followed-by-author" "@author Foo\n@author Bar";
    t "followed-by-author-cr-lf" "@author Foo\n@author Bar";
    t "in-author" "@author Foo @author Bar";
    t "in-author-at-start" "@author @author Foo";
    t "preceded-by-paragraph" "foo\n@author Bar";
    t "no-markup" "@author Foo [Bar]";
    t "in-paragraph" "foo @author Bar";
    t "in-code" "[@author Foo]";
    t "in-style" "{b @author Foo}";
    t "in-heading" "{2 @author Foo}";
    t "after-shorthand-list" "- foo\n@author Bar";
    t "in-shorthand-list" "- foo @author Bar";
    t "in-shorthand-list-at-start" "- @author Foo";
    t "in-list-item" "{ul {li foo @author Bar}}";
    t "in-list-item-at-start" "{ul {li @author Foo}}";
    t "in-list-item-on-new-line" "{ul {li foo\n@author Bar}}";
    t "in-list" "{ul @author Foo}";
    t "in-code-block" "{[@author Foo]}";
    t "in-verbatim" "{v @author Foo v}";
    t "after-code-block" "{[foo]} @author Bar";
    t "after-verbatim" "{v foo v} @author Bar";
    t "after-heading" "{2 Foo} @author Bar";
    t "after-list" "{ul {li foo}} @author Bar";
    t "preceded-by-whitespace" " @author Foo Bar";
    t "second-preceded-by-whitespace" "@author Foo\n @author Bar";
    t "prefix" "@authorfoo";
  ];

  "deprecated", [
    t "basic" "@deprecated";
    t "words" "@deprecated foo bar";
    t "multiline" "@deprecated foo\nbar";
    t "paragraphs" "@deprecated foo\n\nbar";
    t "whitespace-only" "@deprecated ";
    t "immediate-newline" "@deprecated\nfoo";
    t "immediate-cr-lf" "@deprecated\r\nfoo";
    t "immediate-blank-line" "@deprecated\n\nfoo";
    t "extra-whitespace" "@deprecated  foo";
    t "followed-by-deprecated" "@deprecated foo\n@deprecated bar";
    t "followed-by-deprecated-cr-lf" "@deprecated foo\r\n@deprecated bar";
    t "nested-in-self" "@deprecated foo @deprecated bar";
    t "nested-in-self-at-start" "@deprecated @deprecated foo";
    t "preceded-by-paragraph" "foo\n@deprecated";
    t "preceded-by-shorthand-list" "- foo\n@deprecated";
    t "with-shorthand-list" "@deprecated - foo";
    t "with-shorthand-list-after-newline" "@deprecated\n- foo";
    t "prefix" "@deprecatedfoo";
    t "after-code-block" "{[foo]} @deprecated";
    t "followed-by-section" "@deprecated foo\n{2 Bar}";
  ];

  "param", [
    t "basic" "@param foo";
    t "bare" "@param";
    t "bare-with-whitespace" "@param ";
    t "immediate-newline" "@param\nfoo";
    t "followed-by-whitespace" "@param foo ";
    t "extra-whitespace" "@param  foo";
    t "words" "@param foo bar baz";
    t "multiline" "@param foo\nbar\nbaz";
    t "paragraphs" "@param foo bar\n\nbaz";
    t "two" "@param foo\n@param bar";
    t "nested" "@param foo @param bar";
    t "preceded-by-paragraph" "foo\n@param bar";
    t "prefix" "@paramfoo";
    t "after-code-block" "{[foo]} @param foo";
  ];

  "raise", [
    t "basic" "@raise Foo";
    t "bare" "@raise";
    t "words" "@raise foo bar baz";
    t "prefix" "@raisefoo";
  ];

  "return", [
    t "basic" "@return";
    t "words" "@return foo bar";
    t "prefix" "@returnfoo";
  ];

  "see", [
    t "url" "@see <foo>";
    t "file" "@see 'foo'";
    t "document" "@see \"foo\"";
    t "bare" "@see";
    t "unterminated-url" "@see <foo";
    t "unterminated-file" "@see 'foo";
    t "unterminated-document" "@see \"foo";
    t "no-space" "@see<foo>";
    t "words" "@see <foo> bar";
    t "prefix" "@seefoo";
    t "after-code-block" "{[foo]} @see <foo>";
    t "url-attempted-nested-closer" "@see <foo>bar>";
    t "file-attempted-nested-closer" "@see 'foo'bar'";
    t "document-attempted-nested-closer" "@see \"foo\"bar\"";
  ];

  "since", [
    t "basic" "@since foo";
    t "bare" "@since";
    t "prefix" "@sincefoo";
    t "with-whitespace" "@since foo bar";
    t "leading-whitespace" "@since  foo";
    t "trailing-whitespace" "@since foo ";
    t "whitespace-only" "@since ";
  ];

  "before", [
    t "basic" "@before Foo";
    t "bare" "@before";
    t "words" "@before foo bar baz";
    t "prefix" "@beforefoo";
  ];

  "version", [
    t "basic" "@version foo";
    t "bare" "@version";
    t "prefix" "@versionfoo";
    t "with-whitespace" "@version foo bar";
    t "leading-whitespace" "@version  foo";
    t "trailing-whitespace" "@version foo ";
    t "whitespace-only" "@version ";
  ];

  "canonical", [
    t "basic" "@canonical Foo";
    t "empty" "@canonical";
    t "whitespace-only" "@canonical ";
    t "extra-whitespace" "@canonical  Foo ";
    t "prefix" "@canonicalfoo";
    (* TODO This should probably be an error of some kind, as Foo Bar is not a
       valid module path. *)
    t "with-whitespace" "@canonical Foo Bar";
  ];

  "inline", [
    t "basic" "@inline";
    t "prefix" "@inlinefoo";
    t "extra-whitespace" "@inline ";
    t "followed-by-junk" "@inline foo";
    t "followed-by-paragraph" "@inline\nfoo";
    t "followed-by-tag" "@inline\n@deprecated";
    t "with-list" "@inline - foo";
  ];

  "open", [
    t "basic" "@open";
    t "prefix" "@openfoo";
    t "extra-whitespace" "@open ";
    t "followed-by-junk" "@open foo";
    t "followed-by-paragraph" "@open\nfoo";
    t "followed-by-tag" "@open\n@deprecated";
    t "with-list" "@open - foo";
  ];

  "closed", [
    t "basic" "@closed";
    t "prefix" "@closedfoo";
    t "extra-whitespace" "@closed ";
    t "followed-by-junk" "@closed foo";
    t "followed-by-paragraph" "@closed\nfoo";
    t "followed-by-tag" "@closed\n@deprecated";
    t "with-list" "@closed - foo";
  ];

  "reference-component-kind", [
    t "no-kind" "{!foo}";
    t "class" "{!class-foo}";
    t "class-type" "{!class-type-foo}";
    t "class-type-alt" "{!classtype-foo}";
    t "constructor" "{!constructor-Foo}";
    t "constructor-alt" "{!const-Foo}";
    t "exception" "{!exception-Foo}";
    t "exception-alt" "{!exn-Foo}";
    t "extension" "{!extension-Foo}";
    t "field" "{!field-foo}";
    t "field-alt" "{!recfield-foo}";
    t "heading" "{!section-foo}";
    t "heading-alt" "{!label-foo}";
    t "instance-variable" "{!instance-variable-foo}";
    t "method" "{!method-foo}";
    t "module" "{!module-Foo}";
    t "module-type" "{!module-type-Foo}";
    t "module-type-alt" "{!modtype-Foo}";
    t "page" "{!page-foo}";
    t "type" "{!type-foo}";
    t "val" "{!val-foo}";
    t "val-alt" "{!value-foo}";
    t "longident" "{!module-Foo.type-bar}";
    t "hyphenated-kind-longident" "{!module-type-Foo.module-type-Bar.type-baz}";
    t "empty" "{!}";
    t "empty-qualifier" "{!-foo}";
    t "empty-identifier" "{!val-}";
    t "invalid-qualifier" "{!foo-bar}";
    t "empty-first-component" "{!.foo}";
    t "empty-second-component" "{!Foo.}";
    t "second-component-empty-qualifier" "{!Foo.-bar}";
    t "second-component-empty-identifier" "{!Foo.val-}";
    t "first-component-empty-identifier" "{!module-.foo}";
    t "something-in-invalid" "{!foo-bar.baz}";
    t "something-in-something" "{!foo.bar}";
    t "something-in-module" "{!module-Foo.bar}";
    t "something-in-module-type" "{!module-type-Foo.bar}";
    t "something-in-type" "{!type-foo.bar}";
    t "something-in-class" "{!class-foo.bar}";
    t "something-in-class-type" "{!class-type-foo.bar}";
    t "something-in-page" "{!page-foo.bar}";
    t "something-in-constructor" "{!constructor-Foo.bar}";
    t "something-in-exception" "{!exception-Foo.bar}";
    t "something-in-extension" "{!extension-Foo.bar}";
    t "something-in-field" "{!field-foo.bar}";
    t "something-in-section" "{!section-foo.bar}";
    t "something-in-instance-variable" "{!instance-variable-foo.bar}";
    t "something-in-method" "{!method-foo.bar}";
    t "something-in-val" "{!val-foo.bar}";
    t "something-in-something-nested" "{!foo.bar.baz}";
    t "something-in-module-nested" "{!Foo.module-Bar.baz}";
    t "something-in-module-type-nested" "{!Foo.module-type-Bar.baz}";
    t "something-in-type-nested" "{!Foo.type-bar.baz}";
    t "something-in-class-nested" "{!Foo.class-bar.baz}";
    t "something-in-class-type-nested" "{!foo.class-type-bar.baz}";
    t "something-in-page-nested" "{!foo.page-bar.baz}";
    t "something-in-constructor-nested" "{!Foo.constructor-Bar.baz}";
    t "something-in-exception.nested" "{!Foo.exception-bar.baz}";
    t "something-in-extension-nested" "{!Foo.extension-bar.baz}";
    t "something-in-field-nested" "{!foo.field-bar.baz}";
    t "something-in-section-nested" "{!foo.section-bar.baz}";
    t "something-in-instance-variable-nested"
      "{!foo.instance-variable-bar.baz}";
    t "something-in-method-nested" "{!foo.method-bar.baz}";
    t "something-in-val-nested" "{!Foo.val-bar.baz}";
    t "module-in-empty" "{!.module-Foo}";
    t "module-in-something" "{!Foo.module-Bar}";
    t "module-in-module" "{!module-Foo.module-Bar}";
    t "module-in-module-type" "{!module-type-Foo.module-Bar}";
    t "module-in-class" "{!class-foo.module-Bar}";
    t "module-in-class-type" "{!class-type-foo.module-Bar}";
    t "module-in-constructor" "{!constructor-Foo.module-Bar}";
    t "module-in-exception" "{!exception-Foo.module-Bar}";
    t "module-in-extension" "{!extension-Foo.module-Bar}";
    t "module-in-field" "{!field-foo.module-Bar}";
    t "module-in-section" "{!section-foo.module-Bar}";
    t "module-in-instance-variable" "{!instance-variable-foo.module-Bar}";
    t "module-in-method" "{!method-foo.module-Bar}";
    t "module-in-page" "{!page-foo.module-Bar}";
    t "module-in-type" "{!type-foo.module-Bar}";
    t "module-in-val" "{!val-foo.module-Bar}";
    t "module-in-something-nested" "{!Foo.Bar.module-Baz}";
    t "module-in-module-nested" "{!Foo.module-Bar.module-Baz}";
    t "module-in-module-type-nested" "{!Foo.module-type-Bar.module-Baz}";
    t "module-in-class-nested" "{!Foo.class-bar.module-Baz}";
    t "module-in-class-type-nested" "{!Foo.class-type-bar.module-Baz}";
    t "module-in-constructor-nested" "{!Foo.constructor-Bar.module-Baz}";
    t "module-in-exception-nested" "{!Foo.exception-Bar.module-Baz}";
    t "module-in-extension-nested" "{!Foo.extension-Bar.module-Baz}";
    t "module-in-field-nested" "{!foo.field-bar.module-Baz}";
    t "module-in-section-nested" "{!foo.section-bar.module-Baz}";
    t "module-in-instance-variable-nested"
      "{!foo.instance-variable-bar.module-Baz}";
    t "module-in-method-nested" "{!foo.method-bar.module-Baz}";
    t "module-in-page-nested" "{!foo.page-bar.module-Baz}";
    t "module-in-type-nested" "{!Foo.type-bar.module-Baz}";
    t "module-in-val-nested" "{!Foo.val-bar.module-Baz}";
    t "module-type-in-something" "{!Foo.module-type-Bar}";
    t "module-type-in-module" "{!module-Foo.module-type-Bar}";
    t "module-type-in-module-type" "{!module-type-Foo.module-type-Bar}";
    t "module-type-in-class" "{!class-foo.module-type-Bar}";
    t "module-type-in-page" "{!page-foo.module-type-Bar}";
    t "type-in-something" "{!Foo.type-bar}";
    t "type-in-module" "{!module-Foo.type-bar}";
    t "type-in-module-type" "{!module-type-Foo.type-bar}";
    t "type-in-class" "{!class-foo.type-bar}";
    t "type-in-page" "{!page-foo.type-bar}";
    t "constructor-in-empty" "{!.constructor-Foo}";
    t "constructor-in-something" "{!foo.constructor-Bar}";
    t "constructor-in-type" "{!type-foo.constructor-Bar}";
    t "constructor-in-class" "{!class-foo.constructor-Bar}";
    t "constructor-in-class-type" "{!class-type-foo.constructor-Bar}";
    t "constructor-in-constructor" "{!constructor-Foo.constructor-Bar}";
    t "constructor-in-exception" "{!exception-Foo.constructor-Bar}";
    t "constructor-in-extension" "{!extension-Foo.constructor-Bar}";
    t "constructor-in-field" "{!field-foo.constructor-Bar}";
    t "constructor-in-section" "{!section-foo.constructor-Bar}";
    t "constructor-in-instance-variable"
      "{!instance-variable-foo.constructor-Bar}";
    t "constructor-in-method" "{!method-foo.constructor-Bar}";
    t "constructor-in-module" "{!module-Foo.constructor-Bar}";
    t "constructor-in-module-type" "{!module-type-Foo.constructor-Bar}";
    t "constructor-in-page" "{!page-foo.constructor-Bar}";
    t "constructor-in-val" "{!val-foo.constructor-Bar}";
    t "constructor-in-something-nested" "{!foo.bar.constructor-Baz}";
    t "constructor-in-type-nested" "{!foo.type-bar.constructor-Baz}";
    t "constructor-in-class-nested" "{!Foo.class-bar.constructor-Baz}";
    t "constructor-in-class-type-nested"
      "{!Foo.class-type-bar.constructor-Baz}";
    t "constructor-in-constructor-nested"
      "{!Foo.constructor-Bar.constructor-Baz}";
    t "constructor-in-exception-nested" "{!Foo.exception-Bar.constructor-Baz}";
    t "constructor-in-extension-nested" "{!Foo.extension-Bar.constructor-Baz}";
    t "constructor-in-field-nested" "{!foo.field-bar.constructor-Baz}";
    t "constructor-in-section-nested" "{!foo.section-bar.constructor-Baz}";
    t "constructor-in-instance-variable-nested"
      "{!foo.instance-variable-bar.constructor-Baz}";
    t "constructor-in-method-nested" "{!foo.method-bar.constructor-Baz}";
    t "constructor-in-module-nested" "{!Foo.module-Bar.constructor-Baz}";
    t "constructor-in-module-type-nested"
      "{!Foo.module-type-Bar.constructor-Baz}";
    t "constructor-in-page-nested" "{!foo.page-bar.constructor-Baz}";
    t "constructor-in-val-nested" "{!Foo.val-bar.constructor-Baz}";
    t "field-in-empty" "{!.field-foo}";
    t "field-in-something" "{!foo.field-bar}";
    t "field-in-module" "{!module-Foo.field-bar}";
    t "field-in-module-type" "{!module-type-Foo.field-bar}";
    t "field-in-type" "{!type-foo.field-bar}";
    t "field-in-class" "{!class-foo.field-bar}";
    t "field-in-class-type" "{!class-type-foo.field-bar}";
    t "field-in-constructor" "{!constructor-Foo.field-bar}";
    t "field-in-exception" "{!exception-Foo.field-bar}";
    t "field-in-extension" "{!extension-Foo.field-bar}";
    t "field-in-field" "{!field-foo.field-bar}";
    t "field-in-section" "{!section-foo.field-bar}";
    t "field-in-instance-variable" "{!instance-variable-foo.field-bar}";
    t "field-in-method" "{!method-foo.field-bar}";
    t "field-in-page" "{!page-foo.field-bar}";
    t "field-in-val" "{!val-foo.field-bar}";
    t "field-in-something-nested" "{!foo.bar.field-baz}";
    t "field-in-module-nested" "{!Foo.module-Bar.field-baz}";
    t "field-in-module-type-nested" "{!Foo.module-type-Bar.field-baz}";
    t "field-in-type-nested" "{!Foo.type-bar.field-baz}";
    t "field-in-class-nested" "{!Foo.class-bar.field-baz}";
    t "field-in-class-type-nested" "{!Foo.class-type-bar.field-baz}";
    t "field-in-constructor-nested" "{!Foo.constructor-bar.field-baz}";
    t "field-in-exception-nested" "{!Foo.exception-Bar.field-baz}";
    t "field-in-extension-nested" "{!Foo.extension-Bar.field-baz}";
    t "field-in-field-nested" "{!Foo.field-bar.field-baz}";
    t "field-in-section-nested" "{!foo.section-bar.field-baz}";
    t "field-in-instance-variable-nested"
      "{!foo.instance-variable-bar.field-baz}";
    t "field-in-method-nested" "{!foo.method-bar.field-baz}";
    t "field-in-page-nested" "{!foo.page-bar.field-baz}";
    t "field-in-val-nested" "{!Foo.val-bar.field-baz}";
    t "exception-in-something" "{!Foo.exception-Bar}";
    t "exception-in-module" "{!module-Foo.exception-Bar}";
    t "exception-in-class" "{!class-foo.exception-Bar}";
    t "exception-in-page" "{!page-foo.exception-Bar}";
    t "extension-in-something" "{!Foo.extension-Bar}";
    t "extension-in-module" "{!module-Foo.extension-Bar}";
    t "extension-in-class" "{!class-foo.extension-Bar}";
    t "extension-in-page" "{!page-foo.extension-Bar}";
    t "val-in-something" "{!Foo.val-bar}";
    t "val-in-module" "{!module-Foo.val-bar}";
    t "val-in-class" "{!class-foo.val-bar}";
    t "val-in-page" "{!page-foo.val-bar}";
    t "class-in-something" "{!Foo.class-bar}";
    t "class-in-module" "{!module-Foo.class-bar}";
    t "class-in-class" "{!class-foo.class-bar}";
    t "class-in-page" "{!page-foo.class-bar}";
    t "class-type-in-something" "{!Foo.class-type-bar}";
    t "class-type-in-module" "{!module-Foo.class-type-bar}";
    t "class-type-in-class" "{!class-foo.class-type-bar}";
    t "class-type-in-page" "{!page-foo.class-type-bar}";
    t "method-in-empty" "{!.method-foo}";
    t "method-in-something" "{!foo.method-bar}";
    t "method-in-class" "{!class-foo.method-bar}";
    t "method-in-class-type" "{!class-type-foo.method-bar}";
    t "method-in-constructor" "{!constructor-Foo.method-bar}";
    t "method-in-exception" "{!exception-Foo.method-bar}";
    t "method-in-extension" "{!extension-Foo.method-bar}";
    t "method-in-field" "{!field-foo.method-bar}";
    t "method-in-section" "{!section-foo.method-bar}";
    t "method-in-instance-variable" "{!instance-variable-foo.method-bar}";
    t "method-in-method" "{!method-foo.method-bar}";
    t "method-in-module" "{!module-Foo.method-bar}";
    t "method-in-module-type" "{!module-type-Foo.method-bar}";
    t "method-in-page" "{!page-foo.method-bar}";
    t "method-in-type" "{!type-foo.method-bar}";
    t "method-in-val" "{!val-foo.method-bar}";
    t "method-in-something-nested" "{!foo.bar.method-baz}";
    t "method-in-class-nested" "{!Foo.class-bar.method-baz}";
    t "method-in-class-type-nested" "{!Foo.class-type-bar.method-baz}";
    t "method-in-constructor-nested" "{!foo.constructor-Bar.method-baz}";
    t "method-in-exception-nested" "{!Foo.exception-Bar.method-baz}";
    t "method-in-extension-nested" "{!Foo.extension-Bar.method-baz}";
    t "method-in-field-nested" "{!foo.field-bar.method-baz}";
    t "method-in-section-nested" "{!foo.section-bar.method-baz}";
    t "method-in-instance-variable-nested"
      "{!foo.instance-variable-bar.method-baz}";
    t "method-in-method-nested" "{!foo.method-bar.method-baz}";
    t "method-in-module-nested" "{!Foo.module-Bar.method-baz}";
    t "method-in-module-type-nested" "{!Foo.module-type-Bar.method-baz}";
    t "method-in-page-nested" "{!foo.page-bar.method-baz}";
    t "method-in-type-nested" "{!Foo.type-bar.method-baz}";
    t "method-in-val-nested" "{!Foo.val-bar.method-baz}";
    t "instance-variable-in-something" "{!Foo.instance-variable-bar}";
    t "instance-variable-in-module" "{!module-Foo.instance-variable-bar}";
    t "instance-variable-in-class" "{!class-foo.instance-variable-bar}";
    t "instance-variable-in-page" "{!page-foo.instance-variable-bar}";
    t "section-in-something" "{!Foo.section-bar}";
    t "section-in-module" "{!module-Foo.section-bar}";
    t "section-in-class" "{!class-foo.section-bar}";
    t "section-in-page" "{!page-foo.section-bar}";
    t "page-in-something" "{!foo.page-bar}";
    t "inner-parent-something-in-something" "{!foo.bar.field-baz}";
    t "inner-parent-something-in-module" "{!module-Foo.bar.field-baz}";
    t "inner-parent-something-in-class" "{!class-foo.bar.field-baz}";
    t "inner-parent-something-in-page" "{!page-foo.bar.field-baz}";
    t "inner-parent-module-in-module" "{!module-Foo.module-Bar.field-baz}";
    t "inner-parent-module-in-class" "{!class-foo.module-Bar.field-baz}";
    t "inner-parent-module-type-in-module"
      "{!module-Foo.module-type-Bar.field-baz}";
    t "inner-parent-module-type-in-class"
      "{!class-foo.module-type-Bar.field-baz}";
    t "inner-parent-type-in-module" "{!module-Foo.type-bar.field-baz}";
    t "inner-parent-type-in-class" "{!class-foo.type-bar.field-baz}";
    t "inner-parent-class-in-module" "{!module-Foo.class-bar.field-baz}";
    t "inner-parent-class-in-class" "{!class-foo.class-bar.field-baz}";
    t "inner-parent-class-type-in-module"
      "{!module-Foo.class-type-bar.field-baz}";
    t "inner-parent-class-type-in-class"
      "{!class-foo.class-type-bar.field-baz}";
    t "inner-label-parent-something-in-something" "{!foo.bar.baz}";
    t "inner-label-parent-something-in-page" "{!page-foo.bar.baz}";
    t "inner-label-parent-module-in-module" "{!module-Foo.module-Bar.baz}";
    t "inner-label-parent-module-in-class" "{!class-foo.module-Bar.baz}";
    t "inner-label-parent-module-type-in-module"
      "{!module-Foo.module-type-Bar.baz}";
    t "inner-label-parent-module-type-in-class"
      "{!class-foo.module-type-Bar.baz}";
    t "inner-label-parent-type-in-module" "{!module-Foo.type-bar.baz}";
    t "inner-label-parent-type-in-class" "{!class-foo.type-bar.baz}";
    t "inner-label-parent-class-in-module" "{!module-Foo.class-bar.baz}";
    t "inner-label-parent-class-in-class" "{!class-foo.class-bar.baz}";
    t "inner-label-parent-class-type-in-module" "{!module-Foo.class-bar.baz}";
    t "inner-label-parent-class-type-in-class" "{!class-foo.class-bar.baz}";
    t "inner-page-in-something" "{!foo.page-bar.baz}";
    t "inner-class-signature-something-in-something" "{!foo.bar.method-baz}";
    t "inner-class-signature-something-in-page" "{!page-foo.bar.method-baz}";
    t "inner-class-signature-class-in-module"
      "{!module-Foo.class-bar.method-baz}";
    t "inner-class-signature-class-in-class"
      "{!class-foo.class-bar.method-baz}";
    t "inner-class-signature-class-type-in-module"
      "{!module-Foo.class-type-bar.method-baz}";
    t "inner-class-signature-class-type-in-class"
      "{!class-foo.class-type-bar.method-baz}";
    t "inner-signature-something-in-something" "{!foo.bar.type-baz}";
    t "inner-signature-something-in-page" "{!page-foo.bar.type-baz}";
    t "inner-signature-module-in-module" "{!module-Foo.module-Bar.type-baz}";
    t "inner-signature-module-in-class" "{!class-foo.module-Bar.type-baz}";
    t "inner-signature-module-type-in-module"
      "{!module-Foo.module-type-Bar.type-baz}";
    t "inner-signature-module-type-in-class"
      "{!class-foo.module-type-Bar.type-baz}";
    t "inner-datatype-something-in-something" "{!foo.bar.constructor-Baz}";
    t "inner-datatype-something-in-page" "{!page-foo.bar.constructor-Baz}";
    t "inner-datatype-type-in-module" "{!module-Foo.type-bar.constructor-Baz}";
    t "inner-datatype-type-in-class" "{!class-foo.type-bar.constructor-Baz}";
    t "kind-conflict" "{!val:type-foo}";
    t "kind-agreement" "{!val:val-foo}";
    t "kind-agreement-alt" "{!value:val-foo}";
    t "canonical-something" "@canonical Foo";
    t "canonical-module" "@canonical module-Foo";
    t "canonical-path" "@canonical Foo.Bar";
    t "canonical-val" "@canonical val-foo";
    t "canonical-bad-parent" "@canonical bar.page-foo";
    t "canonical-empty-component" "@canonical .Foo";
    t "canonical-empty-name" "@canonical Foo.";
    t "internal-whitespace" "{!foo. bar .baz}";
    t "replacement-text-empty-identifier" "{{!val-} foo}";
  ];

  "bad-markup", [
    t "left-brace" "{";
    t "left-brace-with-letter" "{g";
    t "left-brace-with-letters" "{gg";
    t "empty-braces" "{}";
    t "left-space" "{ foo}";
    t "left-spaces" "{  foo}";
    t "left-space-eof" "{ ";
    t "braces-instead-of-brackets" "{foo}";
    t "right-brace" "}";
    t "right-brace-in-paragraph" "foo}";
    t "multiple-right-brace" "foo } bar } baz";
    t "right-brace-in-list-item" "- foo}";
    t "right-brace-in-code-span" "[foo}]";
    t "right-brace-in-code-block" "{[foo}]}";
    t "right-brace-in-verbatim-text" "{v foo} v}";
    t "right-brace-in-author" "@author Foo}";
    t "right-brace-in-deprecated" "@deprecated }";
    t "right-bracket" "]";
    t "right-bracket-in-paragraph" "foo]";
    t "right-bracket-in-shorthand-list" "- foo]";
    t "right-bracket-in-code-span" "[]]";
    t "right-bracket-in-style" "{b]}";
    t "right-bracket-in-verbatim" "{v ] v}";
    t "right-bracket-in-list" "{ul ]}";
    t "right-bracket-in-list-item" "{ul {li ]}}";
    t "right-bracket-in-heading" "{2 ]}";
    t "right-bracket-in-author" "@author Foo]";
    t "at" "@";
    t "cr" "\r";
  ];

  "utf-8", [
    t "lambda" "\xce\xbb";
    t "words" "\xce\xbb \xce\xbb";
    t "no-validation" "\xce";
    t "escapes" "\xce\xbb\\}";
    t "newline" "\xce\xbb \n \xce\xbb";
    t "paragraphs" "\xce\xbb \n\n \xce\xbb";
    t "code-span" "[\xce\xbb]";
    t "minus" "\xce\xbb-\xce\xbb";
    t "shorthand-list" "- \xce\xbb";
    t "styled" "{b \xce\xbb}";
    t "reference-target" "{!\xce\xbb}";
    t "code-block" "{[\xce\xbb]}";
    t "verbatim" "{v \xce\xbb v}";
    t "label" "{2:\xce\xbb Bar}";
    t "author" "@author \xce\xbb";
    t "param" "@param \xce\xbb";
    t "raise" "@raise \xce\xbb";
    t "see" "@see <\xce\xbb>";
    t "since" "@since \xce\xbb";
    t "before" "@before \xce\xbb";
    t "version" "@version \xce\xbb";
    t "right-brace" "\xce\xbb}";
  ];

  "comment-location", [
    t "error-on-first-line" "  @foo"
      ~location:{line = 2; column = 4};
    t "error-on-second-line" "  \n  @foo"
      ~location:{line = 2; column = 4};
  ];

  "unsupported", [
    (* test "index list"
      "{!indexlist}"
      (Ok []); *)
    t "left-alignment" "{L foo}";
    t "center-alignment" "{C foo}";
    t "right-alignment" "{R foo}";
    t "custom-style" "{c foo}";
    t "custom-tag" "@custom";
    t "custom-reference-kind" "{!custom:foo}";
    t "html-tag" "<b>foo</b>";
  ];
]



let expect_directory = "expect"
let actual_root_directory = "_actual"
let test_root = "test/parser"
let promote_script = "promote.sh"

let (//) = Filename.concat

let read_file filename = Markup.file filename |> fst |> Markup.to_string
let write_file filename data = Markup.string data |> Markup.to_file filename

let mkdir directory =
  try Unix.mkdir directory 0o755
  with Unix.Unix_error (Unix.EEXIST, "mkdir", _) -> ()

let suggest_commands script_directory commands =
  let commands = String.concat "\n" commands in

  let script_file = script_directory // promote_script in

  if not (Sys.file_exists script_file) then
    write_file script_file commands;

  Printf.eprintf "\nbash %s\n\n"
    ("_build/default" // test_root // script_directory // promote_script)

let () =
  mkdir actual_root_directory;
  begin
    try Unix.unlink (actual_root_directory // promote_script);
    with _ -> ()
  end;

  let make_parser_test : string -> test_case -> unit Alcotest.test_case =
      fun suite case ->

    let run_test_case () =
      let file_title = case.name in
      let expect_suite_directory = expect_directory // suite in
      let actual_suite_directory = actual_root_directory // suite in
      let expect_file = expect_suite_directory // (file_title ^ ".txt") in
      let actual_file = actual_suite_directory // (file_title ^ ".txt") in

      let actual =
        let {sections_allowed; location; parser_input; _} = case in

        let dummy_filename = "f.ml" in

        let dummy_page =
          let root : Odoc_model.Root.t = {
            package = dummy_filename;
            file = Page dummy_filename;
            digest = String.make 16 '\000';
          }
          in
          `Page (root, Odoc_model.Names.PageName.of_string dummy_filename)
        in

        let location =
          {
            Lexing.pos_fname = dummy_filename;
            pos_lnum = location.line;
            pos_bol = 0;
            pos_cnum = location.column
          }
        in

        Odoc_parser.parse_comment
          ~sections_allowed
          ~containing_definition:dummy_page
          ~location
          ~text:parser_input
        |> fun parser_output ->
          let buffer = Buffer.create 1024 in
          Print.parser_output
            (Format.formatter_of_buffer buffer) parser_output;
          Buffer.contents buffer
      in

      let expected =
        try read_file expect_file
        with Sys_error _ ->
          mkdir actual_suite_directory;
          write_file actual_file actual;

          prerr_newline ();
          prerr_endline case.parser_input;
          prerr_newline ();

          prerr_string actual;

          prerr_endline "\nThe expected output file does not exist.";
          prerr_endline "\nTo create it, run";
          [
            Printf.sprintf "mkdir -p %s"
              (test_root // expect_suite_directory);
            Printf.sprintf "cp %s %s"
              ("_build/default" // test_root // actual_file)
              (test_root // expect_file);
          ]
          |> suggest_commands actual_root_directory;

          Alcotest.fail "expected output file does not exist";
      in

      if actual <> expected then begin
        mkdir actual_suite_directory;
        write_file actual_file actual;

        prerr_newline ();
        prerr_endline case.parser_input;
        prerr_newline ();

        Printf.sprintf "diff -u %s %s" expect_file actual_file
        |> Sys.command
        |> ignore;

        prerr_endline "\nTo replace expected output with actual, run";
        [
          Printf.sprintf "cp %s %s"
            ("_build/default" // test_root // actual_file)
            (test_root // expect_file);
        ]
        |> suggest_commands actual_root_directory;

        Alcotest.fail "document tree incorrect";
      end
    in

    case.name, `Quick, run_test_case
  in

  let scan_for_stale_tests () =
    let directories = Sys.readdir expect_directory in
    directories |> Array.iter begin fun directory ->
      let source_directory = test_root // expect_directory // directory in

      let (_, tests) =
        try List.find (fun (suite_name, _) -> suite_name = directory) tests
        with Not_found ->
          Printf.eprintf
            "\n\nDirectory '%s' does not correspond to a test suite.\n"
            source_directory;
          prerr_endline "To remove it, run";
          suggest_commands actual_root_directory
            [Printf.sprintf "rm -r %s" source_directory];
          exit 1
      in

      let files = Sys.readdir (expect_directory // directory) in
      files |> Array.iter begin fun file ->
        let source_file = source_directory // file in
        let title = String.sub file 0 (String.length file - 4) in

        try List.find (fun {name; _} -> name = title) tests |> ignore
        with Not_found ->
          Printf.eprintf
            "\n\nExpect file '%s' does not correspond to a test.\n" source_file;
          prerr_endline "To remove it, run";
          suggest_commands actual_root_directory
            [Printf.sprintf "rm %s" source_file];
          exit 1
      end
    end
  in
  scan_for_stale_tests ();

  let tests : (unit Alcotest.test) list =
    tests |> List.map (fun (suite_name, test_cases) ->
      suite_name, List.map (make_parser_test suite_name) test_cases)
  in

  Alcotest.run "parser" tests
