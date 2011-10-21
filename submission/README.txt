Author: Jason Hickey   jyh@cs.caltech.edu

This folder contains the source files for the book "Introduction to
Objective Caml."  There are two parts to it.

   1. book.tex, book.pdf: the textbook itself

   2. answers.tex, answers.pdf

      answers to exercises, to be placed online, password-protected,
      for use by instructors.

* FILES

The LaTeX is split into multiple files, one for each chapter, and another for the
exercises for a chapter.  The defs-*.tex files contain macro definitions.

01_intro.tex                  08_expr4_exercises.tex        15_objects2_exercises.tex
02_expr1.tex                  09_exn1.tex                   16_objects3.tex
02_expr1_exercises.tex        09_exn1_exercises.tex         16_objects3_exercises.tex
03_var1.tex                   10_io1.tex                    17_polyclasses.tex
03_var1_exercises.tex         10_io1_exercises.tex          17_polyclasses_exercises.tex
04_patt1.tex                  11_mod1.tex                   A_syntax.tex
04_patt1_exercises.tex        11_mod1_exercises.tex         answers.tex
05_expr2.tex                  12_mod2.tex                   book.tex
05_expr2_exercises.tex        12_mod2_exercises.tex         defs-book.tex
06_expr3.tex                  13_mod3.tex                   defs-listings.tex
06_expr3_exercises.tex        13_mod3_exercises.tex         defs-metaprl.tex
07_refcells.tex               14_objects1.tex               defs-syntax.tex
07_refcells_exercises.tex     14_objects1_exercises.tex
08_expr4.tex                  15_objects2.tex

The illustrations are in .eps files.

animal1.eps            graph1.eps             graphics8.eps          network_stack.eps
c++-object.eps         graphics1.eps          hash.eps               network_stack2.eps
collection.eps         graphics1a.eps         hierarchy.eps          nor2.eps
diamond.eps            graphics2.eps          ice_cream.eps          regular-poly.eps
dllist1.eps            graphics3.eps          inverter.eps           ring.eps
french-mixin.eps       graphics4.eps          kruskal.eps
french-programmer.eps  graphics5.eps          latch.eps
gates.eps              graphics6.eps          nand2.eps

Additional files.

proof.sty
rc.bib

Typeset versions.

book.pdf
answers.pdf

* CONFIGURATION

The LaTeX packages are fairly standard, here is the list.

\usepackage[T1]{fontenc}
\usepackage{textcomp}
\usepackage{times}                    % Times Roman font
\usepackage[scaled=0.8]{luximono}     % LuxiMono for typesetting code
\usepackage{listings}                 % Code typesetting
\usepackage{fancyhdr}
\usepackage{fancyvrb}                 % For verbatim text in code listings
\usepackage{makeidx}                  % Index
\usepackage{proof}
\usepackage{graphicx}
\usepackage{url}
\usepackage[dvipdfm]{hyperref}

The luximono fonts and style file can be found on CTAN.

    http://www.ctan.org/tex-archive/fonts/LuxiMono/

The code doesn't have to be set in LuxiMono, but LuxiMono looks good
in my opinion.  If using another font, it should be a monospace font,
supporting both normal and bold (for emphasizing keywords), and it
_must not_ be Courier, which really looks terrible.

* BUILDING

The usual sequence of LaTeX commands should work.

   % latex book
   % bibtex book
   % makeindex book
   % latex book
   % latex book
   % dvipdfm -o book.pdf book.dvi

The same commands can be used to build the answers, just change "book" to "answers".
