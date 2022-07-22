%%%to find all changes from EngC, search for %%EngD
%%%
%%
%% This is file 'EngD.cls'  %%EngD. changed from EngC
%% EngD LaTeX2e document class  %%EngD changed from EngC
%%
%% copyright (c) 2011 ali woollatt
%%   generated from cambridge7A.cls 2010/09/09, v2.10
%%
%% Bugs (in the case of unchanged files) should be reported to
%% texline@cambridge.org
%%
%% 2011/02/03, v1.10 includes the following changes:
%%   01 changed Theorems,etc, to all small caps
%%   02 removed \spacedheader from remark style (italic) headings
%%   03 footnotes changed to 8/10
%%   04 endnotes changed to 9/11
%%   05 aligned top of part and chapter headings to top of text box
%%   06 justified text in floating boxes
%%   07 changed \topsep to zero in 2nd and 3rd levels of lists
%%   08 added extracts
%%   09 changed indentation of itemized lists
%%  2020/03/03, adapted for EngD. Search for %%EngD
%%
%% This software may only be used in the preparation of journal articles
%% or books or parts of books to be published by Cambridge University Press.
%% Any other use constitutes an infringement of copyright.
%%
%% \CharacterTable
%%  {Upper-case    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z
%%   Lower-case    \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
%%   Digits        \0\1\2\3\4\5\6\7\8\9
%%   Exclamation   \!     Double quote  \"     Hash (number) \#
%%   Dollar        \$     Percent       \%     Ampersand     \&
%%   Acute accent  \'     Left paren    \(     Right paren   \)
%%   Asterisk      \*     Plus          \+     Comma         \,
%%   Minus         \-     Point         \.     Solidus       \/
%%   Colon         \:     Semicolon     \;     Less than     \<
%%   Equals        \=     Greater than  \>     Question mark \?
%%   Commercial at \@     Left bracket  \[     Backslash     \\
%%   Right bracket \]     Circumflex    \^     Underscore    \_
%%   Grave accent  \`     Left brace    \{     Vertical bar  \|
%%   Right brace   \}     Tilde         \~}
%%

\NeedsTeXFormat{LaTeX2e}

\newcommand\version{2011/02/03, v1.10 }
\ProvidesClass{EngD}[\version LaTeX2e document class]  %%EngD

% font sizes for EngD, 244x170mm  %%EngD
% \tiny                        5/6
% \scriptsize                  7/8
% \copyrightsize               8/9
% \indexsize                   8/10 Y (author affiliation size in [multi] Option,
%                                        also Figure x.x size)
% \let\footnotesize\indexsize
% \small                       9/10
% \smallish                    9/11 Y figure caption size
% \let\enotesize\smallish
% \let\tablesize\smallish      9/11 Y
% \bibliosize                  9/12 Y
% \runningheadsize           9.5/11 Y
% \normalsmall                10/12
% \let\listsize\normalsize    10/13 Y
% \Bheadsize                  12/13 Y (also Cheadsize, Box titles)  %%EngD. new
% \Cheadsize                  11/13 Y (also Cheadsize, Box titles)  %%EngD. was Bheadsize in EngC
% \large                      11/14   (also size of Part titles in toc)
% \Aheadsize                  12/13 Y
% \authorsize                 12/14 Y
% \Large                      12/17
% \mainheadingsize            18/20 Y
% \subtitlesize               18/22 Y
% \huge                       24/26 (chapter number and title)
% \parttitlesize              26/29 (title and part title)

\newif\ifAJW@multisty
%\newif\ifAJW@largesty
\newif\ifAJW@indentsty % false
\newif\ifAJW@landscape
\newif\if@openright
\newif\ifAJW@truesize
\newif\ifchapterrule
\newdimen\AJW@indent
\newdimen\AJW@floatindent
\AJW@indent\z@        % zero
\newif\ifsp@nningrule

\usepackage{color,framed}%,graphics

\DeclareOption{multi}{\AJW@multistytrue}

\DeclareOption{oneside}{\relax}
\DeclareOption{twoside}{\@twosidetrue \@mparswitchtrue}
\DeclareOption{onecolumn}{\@twocolumnfalse}
\DeclareOption{twocolumn}{\@twocolumntrue}
\DeclareOption{draft}{\setlength\overfullrule{5\p@}}
\DeclareOption{final}{\setlength\overfullrule{\z@}}
\DeclareOption{openright}{\@openrighttrue}
\DeclareOption{openany}{\@openrightfalse}
\DeclareOption{a4paper}
  {\setlength\paperheight {297mm}%
   \setlength\paperwidth  {210mm}}
\DeclareOption{landscape}
  {\setlength\@tempdima   {\paperheight}%
   \setlength\paperheight {\paperwidth}%
   \setlength\paperwidth  {\@tempdima}%
   \AJW@landscapetrue}
\DeclareOption{truesize}{\AJW@truesizetrue}
\DeclareOption{spanningrule}{\sp@nningruletrue}

\newif\ifprodtf
\DeclareOption{prodtf}{\prodtftrue}

%%EngD. changes begin here 
\DeclareOption{cropmark}{\PassOptionsToPackage{a4,center,cam}{crop}}

\newif\if@croppage   \global\@croppagefalse
\newif\if@stdlayouts \global\@stdlayoutsfalse
\newif\if@nobleed    \global\@nobleedtrue
\newif\if@weblink    \global\@weblinktrue
\newif\if@ruleron\global\@ruleronfalse
\newif\if@RGBlink\global\@RGBlinkfalse

\newif\ifEngD\global\EngDfalse   %%EngD: changed from \newif\ifEngC\global\EngCfalse
\newif\ifsixA\global\sixAfalse

\DeclareOption{print}{\global\@croppagetrue\PassOptionsToPackage{off}{crop}}%
\DeclareOption{proof}{\PassOptionsToPackage{a4,cam,center}{crop}\global\@croppagefalse}%
\DeclareOption{EngD}{\setlength{\paperwidth}{170truemm}\setlength{\paperheight}{244truemm}\global\@stdlayoutstrue\global\EngDtrue\global\sixAfalse}
%%EngD: changed from \DeclareOption{EngC}{\setlength{\paperwidth}{170truemm}\setlength{\paperheight}{244truemm}\global\@stdlayoutstrue\global\EngDtrue\global\sixAfalse}  %%EngD changed from EngC
\DeclareOption{bleed}{\global\@nobleedfalse}
\DeclareOption{noweblink}{\global\@weblinkfalse}
\DeclareOption{AER}{\global\@weblinktrue\global\@croppagetrue\PassOptionsToPackage{off}{crop}}
\DeclareOption{ruleron}{\global\@rulerontrue}
\DeclareOption{rgblink}{\global\@RGBlinktrue}

%\ExecuteOptions{EngD,proof,noweblink}  %%EngD. changed from EngC
%
%\ProcessOptions

\ExecuteOptions{EngD,proof,noweblink,twoside,onecolumn,final,openright}
%%EngD. changed from\ExecuteOptions{EngC,proof,noweblink,twoside,onecolumn,final,openright}
%%EngD changes end here
\ProcessOptions\relax

% HelveticaNeueLTStd-Cn Helvetica Neue LT Std 57 Condensed
% HelveticaNeueLTStd-LtCn Helvetica Neue LT Std 47 Light Condensed
% HelveticaNeueLTStd-MdCn Helvetica Neue LT Std 67 Medium Condensed
% HelveticaNeueLTStd-BdCn Helvetica Neue LT Std 77 Bold Condensed

\ifprodtf
\AtBeginDocument{
  \usepackage{helvneue}%helvetica neue condensed fonts
  %
  \newcommand{\lightseriffont}{\HelvNeueLtCond}
  \newcommand{\seriffont}{\HelvNeueCond}
  \newcommand{\mediumseriffont}{\HelvNeueMedCond}
  \newcommand{\boldseriffont}{\HelvNeueBldCond}
  }
\else
  \newcommand{\lightseriffont}{\sf}
  \newcommand{\seriffont}{\sf}
  \newcommand{\mediumseriffont}{\sf}
  \newcommand{\boldseriffont}{\sf\bfseries}
\fi

% to stop html.sty (via harvard.sty) creating havoc
\let\htmlstyloaded\relax




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\AtEndOfPackage{\AtBeginDocument{%

% 1. to bring amsthm.sty    into line with cambridge style
% 2. to bring multind.sty   into line with cambridge style
% 3. to bring index.sty     into line with cambridge style
% 4. to bring harvard.sty   into line with cambridge style
% 5. to bring rotating.sty  into line with cambridge style
% 6. to bring sidecap.sty   into line with cambridge style

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 1. to bring amsthm.sty into line with cambridge style
%
\newif\if@remark
\def\th@remark{%
  \@remarktrue
  \thm@headfont{\itshape}%
  \normalfont % body font
  \thm@preskip\topsep \divide\thm@preskip\tw@
  \thm@postskip\thm@preskip
}

% replace period after heading with \enskip
%
\@ifpackageloaded{amsthm}{%
\providecommand\proof{}
\renewenvironment{proof}[1][\proofname]{\par
  \pushQED{\qed}%
% \normalfont \topsep6\p@\@plus6\p@\relax
  \topsep0.5\AJW@theskip \@plus 2\p@ \@minus 1\p@\relax
  \trivlist
  \item[\hskip\labelsep
        \itshape
%    #1\@addpunct{.}]\ignorespaces
     #1\enskip]\ignorespaces
}{%
  \popQED\endtrivlist\@endpefalse
}
%
%
% headings to be small caps and letterspaced (using soul.sty)
% \sodef{cmd}{font}{inter-letter space}{word space}{outer space}

  \thm@headfont{\scshape}
% \sodef\spacedheader{}{.06em}{1em plus1em}{2em plus.1em minus.1em}
  \sodef\spacedheader{}{.06em}{0.15em plus.1em minus.1em}{0.5em plus.1em minus.1em}
%

% replace period after heading with \enskip
%
\def\@thm#1#2#3{%
  \ifhmode\unskip\unskip\par\fi
  %\normalfont
  \trivlist
  \let\thmheadnl\relax
  \let\thm@swap\@gobble
  \thm@notefont{\fontseries\mddefault\upshape}%
% \thm@headpunct{.}% add period after heading
  \thm@headpunct{\enskip}% add space after heading
  \thm@headsep 5\p@ plus\p@ minus\p@\relax
  \thm@space@setup
  #1% style overrides
  \@topsep \thm@preskip               % used by thm head
  \@topsepadd \thm@postskip           % used by \@endparenv
  \def\@tempa{#2}\ifx\@empty\@tempa
    \if@remark% italic, non-spaced heads, unnumbered
      \def\@tempa{\@oparg{\@begintheorem{#3}{}}[]}%
    \else
      \def\@tempa{\@oparg{\@begintheorem{\lowercase{\spacedheader{#3}}}{}}[]}%
    \fi
  \else
    \refstepcounter{#2}%
    \if@remark% italic, non-spaced heads
      \def\@tempa{\@oparg{\@begintheorem{#3}%
      \textit{\csname the#2\endcsname}}[]}
    \else% lowercase first letter
      \def\@tempa{\@oparg{\@begintheorem{\lowercase{\spacedheader{#3}}}%
             {\csname the#2\endcsname}}[]}
    \fi
  \fi
  \@tempa
}}{}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 2. to bring multind.sty into line with cambridge style
%      note that an error in multind.sty will stop verbatim environment
%      working; also everything will come out \indexsize
%      to correct this, we've added an extra set of parens
%
% change chapter opening
%
\@ifpackageloaded{multind}{%
  \def\printindex#1#2{{\@restonecoltrue
    \if@twocolumn\@restonecolfalse\fi
    \columnseprule\z@
    \columnsep 1pc%
    \twocolumn[\vspace*{10\p@}\mbox{}\par\@makeschapterhead{#2}]%
    \@mkboth{#2}{#2}%\markright{\uppercase{#2}}
    \typeout{#2}%
    \addcontentsline{toc}{schapter}{#2}%
    \thispagestyle{plain}
    \indexsize\raggedright
    \vspace*{-7pt}    \parindent\z@  %%EngD. \vspace*{-7pt} added
    \parskip\z@ \@plus.3\p@\relax
    \let\item=\@idxitem
    \@input{#1.ind}}}
%
  \def\@idxitem{\par\hangindent 2em}
  \def\subitem{\par\hangindent 3em \hspace*{1em}}
  \def\subsubitem{\par\hangindent 4em \hspace*{2em}}
  \def\indexspace{\par \vskip 3\p@ \@plus 1\p@ \@minus 1\p@\relax}}{}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 3. to bring index.sty into line with cambridge style
%
\@ifpackageloaded{index}{%
  \renewenvironment{theindex}{%
    \edef\indexname{\the\@nameuse{idxtitle@\@indextype}}%
    \if@twocolumn
      \@restonecolfalse
    \else
      \@restonecoltrue
    \fi
    \columnseprule \z@
    \columnsep 1pc
    %\twocolumn[\@makeschapterhead{\indexname}%
    \twocolumn[\vspace*{10\p@}\mbox{}\par\@makeschapterhead{\indexname}
    \ifx\index@prologue\@empty\else
      \index@prologue
      \bigskip
    \fi
  ]%
    % \@mkboth{\MakeUppercase\indexname}%
    % {\MakeUppercase\indexname}%
  \@mkboth{\indexname}%
  {\indexname}%
  \ifAJW@indentsty
    \ifdim\AJW@indent>\z@
      \addcontentsline{toc}{schapter}{\protect\numberline{\mbox{}}\indexname}
    \else
      \addcontentsline{toc}{schapter}{\indexname}
    \fi
  \else
    \addcontentsline{toc}{schapter}{\indexname}
  \fi
  \thispagestyle{plain}%
  \indexsize\raggedright
  \parindent\z@
  \parskip\z@ \@plus .3\p@\relax
  \let\item\@idxitem
  }{%
  \if@restonecol
    \onecolumn
  \else
    \clearpage
  \fi
  }}%
{}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 4. to bring harvard.sty into line with cambridge style
%    and make 'References' the default
%
\@ifpackageloaded{harvard}{%
%
\renewcommand{\harvardurl}[1]{\textbf{URL:}\textit{##1}}
%
%\bibhang=4mm%   %%EngD; added
%
\renewenvironment{thebibliography}[1]
     {%
     \ifAJW@multisty
       \section*{\refname}%
     \else
       \chapter*{\refname}%
     \fi
     \@mkboth{\refname}{\refname}%
     \list{\@biblabel{\@arabic\c@enumiv}}%
          {\vskip0.9pt\bibliosize%\settowidth\labelwidth{\@biblabel{#1}}%  %%EngD
           \itemindent -1.29em\leftmargin 1.29em\labelwidth \z@ \labelsep \z@%  %%EngD
           \@openbib@code
           \usecounter{enumiv}%
           \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
     \sloppy
     \clubpenalty4000
     \@clubpenalty \clubpenalty
     \widowpenalty4000%
     \sfcode`\.\@m}
    {\def\@noitemerr
      {\@latex@warning{Empty `thebibliography' environment}}%
     \endlist}
}{}
% to add references at the end of a chapter
\newcommand\chapterreferences{\renewcommand\bibsection{\section*{\refname}}%
  \addcontentsline{toc}{section}{\refname}}

% to add references at the end of the book
\newcommand\bookreferences{\renewcommand\bibsection{\chapter*{\refname}}%
  \addcontentsline{toc}{chapter}{\refname}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 5. to bring rotating.sty into line with cambridge style
%
%
% \rotFPtop        \rotFPbot          Effect
% 0pt plus 1fil    0pt plus 1fil      figure/table appears in middle (default value)
% 0pt              0pt plus 1fil      figure/table appears with its top nearest the edge of the page
% 0pt plus 1fil    0pt plus 2fil      figure/table�s bottom appears twice as far from the edge as the top does

\@ifpackageloaded{rotating}{%
  \let\@makecaption=\make@table@caption
  %\let\@makecaption=\make@figure@caption
  \def\sidewaystable{\@rotfloat{table}\smallish}%
  \renewenvironment{sidewaystable*}%
    {\@rotdblfloat{table}\smallish}%
    {\end@rotdblfloat}%
  \rotFPtop 0pt plus 1fil%
  \rotFPbot 0pt plus 1fil%
  }%
  {}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 6. to bring sidecap.sty into line with cambridge style
%

\@ifpackageloaded{sidecap}{%
  \renewcommand\sidecaptionsep{1pc}%
%  \newlength\abovecaptionskip%\setlength\abovecaptionskip{40pt}%
%  \newlength\belowcaptionskip%\setlength\belowcaptionskip{10pt}%
  \renewenvironment{SCtable}{\tablesize\SC@float[\SC@table@vpos]{table}}{\endSC@float}%
  \renewenvironment{SCtable*}{\tablesize\SC@dblfloat[\SC@table@vpos]{table}}{\endSC@dblfloat}%
  \renewenvironment{SCfigure}{\tablesize\SC@float[\SC@figure@vpos]{figure}}{\endSC@float}%
  \renewenvironment{SCfigure*}{\tablesize\SC@dblfloat[\SC@figure@vpos]{figure}}{\endSC@dblfloat}}
  {}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
}}% end of \AtEndOfPackage section
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% \newcommand\cls@pttxt[1]{\typeout{cambridge7A.cls, [multi] option}}
% \newcommand\CUP@five{\ifAJW@multisty \cls@pttxt{5b}\else \cls@pttxt{5a}\fi}
% \newcommand\CUP@six{\ifAJW@multisty \cls@pttxt{6b}\else \cls@pttxt{6a}\fi}
% \newcommand\CUP@seven{\ifAJW@multisty \cls@pttxt{cambridge7B}\fi}
% \newcommand\CUP@eight{\ifAJW@multisty \cls@pttxt{8b}\else \cls@pttxt{8a}\fi}
% \newcommand\CUP@nine{\ifAJW@multisty \cls@pttxt{9b}\else \cls@pttxt{9a}\fi}

% \ifAJW@largesty
% \ifAJW@indentsty \CUP@five\else \CUP@six\fi
% \else
% \ifAJW@indentsty {\ifdim\AJW@indent=\z@ \CUP@nine\else \CUP@eight\fi}%
% \else
%   \CUP@seven
% \fi
% \fi

\newif\if@mainmatter \@mainmattertrue
\newcommand\frontmatter{\cleardoublepage
            \@mainmatterfalse\pagenumbering{roman}}
\newcommand\mainmatter{\cleardoublepage
       \@mainmattertrue\pagenumbering{arabic}}
\newcommand\backmatter{\if@openright\cleardoublepage\else\clearpage\fi
      \@mainmatterfalse}

%  ****************************************
%  *              SHADING                 *
%  ****************************************

\definecolor{thirtycolor}{gray}{.7}    %    30% black for greychapterrule


%  ****************************************
%  *               FONTS                  *
%  ****************************************
%
\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand\baselinestretch{}
\newskip\AJW@baseskip
\newskip\AJW@theskip
\def\AJW@setskips#1{\AJW@theskip #1\relax%
  \abovedisplayskip      0.50\AJW@theskip \@plus 0.25\AJW@theskip \@minus 1\p@%
  \belowdisplayskip      \abovedisplayskip
  \abovedisplayshortskip 0.25\AJW@theskip \@plus 0.25\AJW@theskip
  \belowdisplayshortskip 0.50\AJW@theskip \@plus 0.25\AJW@theskip \@minus 1\p@%
}
\def\AJW@setlists#1{\AJW@theskip #1\relax%
 \def\@listi{\leftmargin\leftmargini
  \topsep  0.5\AJW@theskip \@plus 2\p@ \@minus 1\p@%
  \parsep  \z@
  \itemsep \z@}}
%
%
% common Sizes
\newcommand{\@ixptvpt}{9.5}%                  9.5pt size
  \DeclareMathSizes{\@ixptvpt}{\@ixptvpt}{\@viipt}{\@vipt}
\newcommand{\@xvipt}{16}%                     16pt size
  \DeclareMathSizes{\@xvipt}{\@xvipt}{\@xiipt}{\@xpt}
\newcommand{\@xviiipt}{18}%                   18pt size
  \DeclareMathSizes{\@xviiipt}{\@xviiipt}{\@xiipt}{\@xpt}
\newcommand{\@xxivpt}{24}%                    24pt size
  \DeclareMathSizes{\@xxivpt}{\@xxivpt}{\@xxpt}{\@xvipt}
\newcommand{\@xxvipt}{26}%                    26pt size
  \DeclareMathSizes{\@xxvipt}{\@xxvipt}{\@xxpt}{\@xvipt}

\newcommand\tiny{\@setfontsize\tiny\@vpt{6}\AJW@setskips\AJW@setlists}
\newcommand\copyrightsize{\AJW@baseskip 9pt%
  \@setfontsize\copyrightsize\@viiipt{9}\AJW@setskips\AJW@setlists}
\newcommand\indexsize{\AJW@baseskip 10pt%
  \@setfontsize\indexsize\@viiipt{10}\AJW@setskips\AJW@setlists}
\let\footnotesize\indexsize
\newcommand\bibliosize{\AJW@baseskip 12pt%
  \@setfontsize\bibliosize\@ixpt{12}\AJW@setskips\AJW@setlists}
\newcommand\runningheadsize{\AJW@baseskip 11pt%
  \@setfontsize\runningheadsize{9.5bp}{11}\AJW@setskips\AJW@setlists}  %%EngD
\newcommand\Bheadsize{\@setfontsize\Bheadsize\@xiipt{13}\AJW@setskips\AJW@setlists} %%EngD
\newcommand\Cheadsize{\@setfontsize\Bheadsize\@xipt{13}\AJW@setskips\AJW@setlists}  %%EngD
\newcommand\large{\@setfontsize\large\@xipt{14}\AJW@setskips\AJW@setlists}
\newcommand\Aheadsize{\@setfontsize\Aheadsize\@xiipt{13}\AJW@setskips\AJW@setlists}
\newcommand\authorsize{\@setfontsize\authorsize\@xiipt{14}\AJW@setskips\AJW@setlists}
\newcommand\Large{\@setfontsize\Large\@xiipt{17}\AJW@setskips\AJW@setlists}
\newcommand\mainheadingsize{\@setfontsize\mainheadingsize\@xviiipt{20}\AJW@setskips\AJW@setlists}
\newcommand\subtitlesize{\@setfontsize\subtitlesize\@xviiipt{22}\AJW@setskips\AJW@setlists}
\newcommand\huge{\@setfontsize\huge\@xxivpt{26}\AJW@setskips\AJW@setlists}
\newcommand\parttitlesize{\@setfontsize\parttitlesize\@xxvipt{29}\AJW@setskips\AJW@setlists}

\newcommand\scriptsize{%
  \@setfontsize\scriptsize\@viipt{8}\AJW@setskips\AJW@setlists}
\newcommand\small{\AJW@baseskip 10pt%
  \@setfontsize\small\@ixpt{10}\AJW@setskips\AJW@setlists}
\newcommand\smallish{\AJW@baseskip 11pt%
  \@setfontsize\smallish\@ixpt{11}\AJW@setskips\AJW@setlists}
\let\enotesize\smallish
\let\tablesize\smallish
\newcommand\normalsmall{\AJW@baseskip 12pt%
  \@setfontsize\normalsmall\@xpt{12}\AJW@setskips\AJW@setlists}
\renewcommand\normalsize{\AJW@baseskip 13pt%
  \@setfontsize\normalsize\@xpt{13}\AJW@setskips{\AJW@baseskip}%
  \let\@listi\@listI}
\let\listsize\normalsize

\normalsize

\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*\cal{\@fontswitch{\relax}{\mathcal}}
\DeclareRobustCommand*\mit{\@fontswitch{\relax}{\mathnormal}}

% define a smallverbatim 8/10

\begingroup \catcode `|=0 \catcode `[= 1
\catcode`]=2 \catcode `\{=12 \catcode `\}=12
\catcode`\\=12 |gdef|@xsmallverbatim#1\end{smallverbatim}[#1|end[smallverbatim]]
|gdef|@sxsmallverbatim#1\end{smallverbatim*}[#1|end[smallverbatim*]]
|endgroup

\def\@smallverbatim{\trivlist \item\relax
  \if@minipage\else\vskip\parskip\fi
  \leftskip\@totalleftmargin\rightskip\z@skip
  \parindent\z@\parfillskip\@flushglue\parskip\z@skip
  \@@par
  \@tempswafalse
  \def\par{%
    \if@tempswa
      \leavevmode \null \@@par\penalty\interlinepenalty
    \else
      \@tempswatrue
      \ifhmode\@@par\penalty\interlinepenalty\fi
    \fi}%
  \let\do\@makeother \dospecials
  \obeylines \smallverbatim@font \@noligs
  \hyphenchar\font\m@ne
  \everypar \expandafter{\the\everypar \unpenalty}%
}
\def\smallverbatim{\@smallverbatim \frenchspacing\@vobeyspaces \@xsmallverbatim}
\def\endsmallverbatim{\if@newlist \leavevmode\fi\endtrivlist}

\def\smallverbatim@font{\normalfont\indexsize\ttfamily}


%  ****************************************
%  *            PAGE LAYOUT               *
%  ****************************************
%
% All margin dimensions measured from a point one inch from top and side
% of page.
%
% SIDE MARGINS:
%
  \setlength\oddsidemargin{48pt}
  \setlength\evensidemargin{97pt}
  \setlength\marginparwidth{91pt}%evenside minus 6pt
  \setlength\marginparsep{6pt}
%\ifAJW@largesty
%  \setlength\topmargin{34\p@}
%  \setlength\headheight{14\p@}
% %% \setlength\footheight{14\p@}
%  \setlength\footskip{28\p@}
%  \setlength\topskip{11\p@}
%\else
  \setlength\topmargin{29\p@}
  \setlength\headheight{13\p@}
%%  \setlength\footheight{13\p@}
  \setlength\footskip{26\p@}
  \setlength\topskip{10\p@}
%\fi
  \setlength\headsep{38\p@}
%
% DIMENSION OF TEXT:
%
\newdimen\AJW@measure
\setlength\textheight{43\baselineskip}
\addtolength\textheight{\topskip}

%\ifAJW@largesty
%  \AJW@measure 30pc
%\else
   \AJW@measure 29pc
%\fi
\textwidth\AJW@measure
\ifAJW@landscape
  \textwidth\textheight
  \textheight\AJW@measure
\fi
\setlength\columnsep{1pc}
\setlength\columnseprule{0\p@}


%%EngD changes begin here
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Page Layout
\setlength\paperheight{244mm}
\setlength\paperwidth{170mm}

\setlength\oddsidemargin   {16mm}
\addtolength{\oddsidemargin}{-1in}
\addtolength{\oddsidemargin}{17mm}

\setlength\evensidemargin  {\paperwidth}
\addtolength{\evensidemargin}{-\textwidth}
\addtolength{\evensidemargin}{-\oddsidemargin}
\addtolength{\evensidemargin}{-2in}
\addtolength{\evensidemargin}{2.27mm}
\addtolength{\evensidemargin}{17mm}

\AJW@measure 120mm%29pc

\let\textwidth\AJW@measure%

\setlength{\textheight}{43\baselineskip}
\setlength\textheight{43\baselineskip}
\addtolength\textheight{0.7\topskip}
\addtolength\textheight{5.5pt}

\setlength\topmargin{12.5mm}%
\addtolength{\topmargin}{-1in}

\setlength\headheight{6\p@}
\setlength\topskip   {10\p@}

\setlength\headsep   {11.73mm}

\parskip=0pt%
\setlength\maxdepth{0pt}%
\setlength\footskip{18.2\p@}

\setlength\parskip{0\p@}
\setlength\partopsep{0\p@}

\setlength\parindent{4mm}

\setcounter{secnumdepth}{4}

\RequirePackage[dvips]{crop}
%\if@croppage%
%\CROP@size{\paperwidth}{\paperheight}
%\fi%

\def\rmdefault{ptm}

%\usepackage{newtxmath}   %%EngD: note. why add this here; should be called near start




\def\rmdefault{ptm}

\DeclareSymbolFont{num}{\encodingdefault}{\rmdefault}{m}{n}
\SetSymbolFont{num}{normal}{\encodingdefault}{\rmdefault}{m}{n}
\SetSymbolFont{num}{bold}{\encodingdefault}{\rmdefault}{b}{n}
%
\DeclareMathSymbol{0}{\mathalpha}{num}{"30}
\DeclareMathSymbol{1}{\mathalpha}{num}{"31}
\DeclareMathSymbol{2}{\mathalpha}{num}{"32}
\DeclareMathSymbol{3}{\mathalpha}{num}{"33}
\DeclareMathSymbol{4}{\mathalpha}{num}{"34}
\DeclareMathSymbol{5}{\mathalpha}{num}{"35}
\DeclareMathSymbol{6}{\mathalpha}{num}{"36}
\DeclareMathSymbol{7}{\mathalpha}{num}{"37}
\DeclareMathSymbol{8}{\mathalpha}{num}{"38}
\DeclareMathSymbol{9}{\mathalpha}{num}{"39}

%%EngD changes end here
%
% FOOTNOTES
%
\setlength\footnotesep{6.5\p@}
\setlength{\skip\footins}{9\p@ \@plus 4\p@ \@minus 2\p@}% top of first footnote.



%
% FLOATS
%
\setlength\floatsep{13\p@ \@plus 2\p@ \@minus 2\p@}
\setlength\textfloatsep{20\p@ \@plus 2\p@ \@minus 4\p@}
\setlength\intextsep{13\p@ \@plus 2\p@ \@minus 2\p@}
%%\setlength\@maxsep{20\p@}
\setlength\dblfloatsep{13\p@ \@plus 2\p@ \@minus 2\p@}
\setlength\dbltextfloatsep{20\p@ \@plus 2\p@ \@minus 4\p@}
%%\setlength\@dblmaxsep{20\p@}
%
% for floats on a separate float page
\setlength\@fptop{0\p@ \@plus 1fil}
\setlength\@fpsep{13\p@ \@plus 2fil}
\setlength\@fpbot{0\p@ \@plus 1fil}

%
%   wide floats
\setlength\@dblfptop{0\p@ \@plus 0fil}
\setlength\@dblfpsep{13\p@ \@plus 0fil}
\setlength\@dblfpbot{0\p@ \@plus 3fil}
%
%
% MARGINAL NOTES:
%
\setlength\marginparpush{5\p@}

%  ****************************************
%  *           PARAGRAPHING               *
%  ****************************************
%
\setlength\parskip{0\p@ \@plus .25\p@}
\setlength\parindent{1em}
\listparindent\parindent
\setlength\partopsep{0\p@ \@plus 1\p@}
%
% The following page-breaking penalties are defined
%
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\@beginparpenalty -\@lowpenalty
\@endparpenalty   -\@lowpenalty
\@itempenalty     -\@lowpenalty
%
  \clubpenalty=0       % 'Club line'  at bottom of page.
  \widowpenalty=10000  % 'Widow line' at top of page.
% \displaywidowpenalty % Math display widow line.
% \predisplaypenalty   % Breaking before a math display.
% \postdisplaypenalty  % Breaking after a math display.
% \interlinepenalty    % Breaking at a line within a paragraph.
% \brokenpenalty       % Breaking after a hyphenated line.

%    ****************************************
%    *        CHAPTERS AND SECTIONS         *
%    ****************************************
%
% Heading for the \chapter command -- fixed depth chapter headings...
%
\newdimen\AJW@headwidth
\AJW@headwidth \textwidth
\addtolength{\AJW@headwidth}{-\AJW@indent}

\newif\ifAJW@appendix
\newif\ifAJW@oneappendix

\newcommand\chapterauthor[1]{%
  \ifAJW@multisty
    \def\@chapterauthor{}%
  \else
    \def\@chapterauthor{#1}%
  \fi
}

\edef\@chapterauthor{\relax}

\newcommand\ch@pterauthor[1]{%
  \gdef\@ch@pterauthor{#1}}

\gdef\@ch@pterauthor{%
  \vbox to0pt{%
  \vspace*{-19.5pt}%
  \parbox[t]{\textwidth}{\seriffont\authorsize\@chapterauthor
  }%
  \vss}%
}

\newcommand\affil[1]{\vspace*{-1pt}\\{\indexsize #1}}  %%EngD changed

% chapter rule


\newlength\fourpica
\setlength\fourpica{17mm}  %%EngD: changes from 4pica

\newcommand\greychapterrule{%
\setlength\fboxsep{2\p@}%
  \def\FrameCommand{\hspace*{-17mm}\colorbox{thirtycolor}}% %%EngD: changed
  \parindent0pt%
% frame needs to be \hsize + 17mm - 2x \fboxsep %%EngD changed
  \MakeFramed{%
    \advance\hsize-\width
    \advance\hsize\fourpica
    \advance\hsize-2\fboxsep
    \FrameRestore}%
  \endMakeFramed
}

\def\@makechapterhead#1{%
  {\normalsize
  \parindent 0\p@
 \vspace*{-2.4mm}  %%EngD changed
  \vbox to 4.2mm{\ifnum \c@secnumdepth >\m@ne  %%EngD changed
       \raisebox{-9pt}[0\p@][0\p@]{\mbox{\hspace*{\AJW@indent}}% %%EngD changed
    \begin{minipage}[t]{\AJW@headwidth}
     \raggedright
     \ifAJW@oneappendix
       \hspace*{-17mm}{\huge\mediumseriffont\appendixname\enskip}% %%EngD changed
     \else
       \ifAJW@appendix
         \hspace*{-17mm}{\huge\mediumseriffont\appendixname\ \thechapter\enskip}%  %%EngD changed
       \else
       \hspace*{-17mm}\hbox to17mm{\huge\boldseriffont\thechapter\hfill}%  %%EngD changed
       \fi
     \fi
     \huge\mediumseriffont #1\nobreak\par
     \vspace*{-18pt}%
     \greychapterrule
     \ifAJW@multisty
      \vskip 2\p@
      \authorsize
      \begin{author@tabular}[t]{@{}l@{}}
        \@author
      \end{author@tabular}
      \par
     \fi
\vskip0.2pt  %%EngD added
     \@ch@pterauthor
    \end{minipage}%
    }
  \else
   \raisebox{0.5\baselineskip}[0\p@][0\p@]{\mbox{\hspace*{\AJW@indent}}%
    \begin{minipage}[t]{\AJW@headwidth}
     \raggedright
     {\huge\mediumseriffont\mbox{}}
     \par
     \vskip 7\p@
     \huge\mediumseriffont #1%
     \nobreak
     \par
    \@ch@pterauthor
    \end{minipage}}
  \fi}  %%EngD needed to add }
  \par\nobreak
  \vskip 8.5\baselineskip% 2.5 less than the first line of text
  }
  \chapterauthor{\relax}%
}

\def\@makeschapterhead#1{{\normalsize
  \parindent 0\p@
\vspace*{-5.25mm}  %%%%EngD changed
\vbox to 46mm{\raisebox{0.5\baselineskip}[0\p@][0\p@]{\mbox{\hspace*{\AJW@indent}}%  %%%%EngD changed
   \begin{minipage}[t]{\AJW@headwidth}
    \raggedright
    {\mainheadingsize\boldseriffont\mbox{}}
    \par
    \mainheadingsize\boldseriffont #1%
    \nobreak
    \par
    \@ch@pterauthor
   \end{minipage}}}  %%EngD needed to add }
  \par \nobreak
%  \vskip 11.3\baselineskip  %%EngD deleted
  }\chapterauthor{\relax}%
}
%
% \secdef{UNSTARCMDS}{STARCMDS} :
%

\newcount\apptmp

\def\chapter{%
   %\c@chapter=\apptmp
   %\stepcounter{chapter}
   \endappendix% in case previous chapter contained appendices
   \clearpage
   \thispagestyle{plain}
   \global\@topnum\z@
   \@afterindentfalse
   \secdef\@chapter\@schapter}
\def\@chapter[#1]#2{%
  \ifnum \c@secnumdepth >\m@ne
    \refstepcounter{chapter}
    \typeout{\@chapapp\space\thechapter.}
    \ifAJW@multisty
      \ifx\@shortauthor\@empty
%
% this code just added
      \ifAJW@oneappendix
        \addcontentsline{toc}{chapter}{\protect\AJW@nonumberline{}#1}
      \else
        \ifAJW@appendix
          \addcontentsline{toc}{chapter}{\protect\AJW@numberline{\thechapter}#1}
        \else
          \addcontentsline{toc}{chapter}{\protect\numberline{\thechapter}#1}
        \fi
      \fi
%
%
%%%%%%%%%\addcontentsline{toc}{chapter}{\protect\numberline{\thechapter}#1}
      \else
        \addcontentsline{toc}{chapter}{\protect
        \numberline{\thechapter}#2\hspace{1em}{\itshape \@shortauthor\/}}% #1 changed to #2
                                                                         % so long version of chapter title
                                                                         % appears in toc
      \fi
    \else %if single-author
      \ifAJW@oneappendix
        \addcontentsline{toc}{chapter}{\protect\AJW@nonumberline{}#1}
      \else
        \ifAJW@appendix
          \addcontentsline{toc}{chapter}{\protect\AJW@numberline{\thechapter}#1}
        \else
          \addcontentsline{toc}{chapter}{\protect\numberline{\thechapter}#1}
        \fi
      \fi
    \fi
  \else
   \addcontentsline{toc}{chapter}{#1}
  \fi
  \ifAJW@multisty
   \markboth{\@shortauthor}{#1}
   \gdef\AJW@endnotehead{\relax}
  \else
   \chaptermark{#1}
   \gdef\AJW@endnotehead{\@chapapp\ \thechapter}
  \fi
  \if@twocolumn
    \@topnewpage[\@makechapterhead{#2}]
   \else
    \@makechapterhead{#2}
    \@afterheading
  \fi
  \ifAJW@multisty
   \gdef\@author{}
   \gdef\@shortauthor{}
  \fi
}

\def\@schapter#1{%
 \ifAJW@addtoc
  \ifAJW@indentsty
   \ifdim\AJW@indent>\z@
    \addcontentsline{toc}{schapter}{\protect\numberline{\mbox{}}#1}
   \else
    \addcontentsline{toc}{schapter}{#1}
   \fi
  \else
   \addcontentsline{toc}{schapter}{#1}
  \fi
 \fi
 \ifAJW@multisty
  \markboth{#1}{#1}
  \gdef\AJW@endnotehead{\relax}
 \else
  \chaptermark{#1}
  \gdef\AJW@endnotehead{#1}
 \fi
 \if@twocolumn
  \@topnewpage[\@makeschapterhead{#1}]
 \else
  \@makeschapterhead{#1}
  \@afterheading
 \fi
 \normalsize
}

%
% \@startsection {NAME}{LEVEL}{INDENT}{BEFORESKIP}{AFTERSKIP}{STYLE}
%            optional * [ALTHEADING]{HEADING}
%
 \def\section{\@startsection {section}{1}{\z@}
  {-26\p@ \@plus-7\p@ \@minus-3.5\p@}{13\p@}{\Aheadsize\boldseriffont\AJW@hangraggedright}}
 \def\subsection{\@startsection{subsection}{2}{\z@}
  {-20.5\p@ \@plus-7\p@ \@minus-3.5\p@}{6.5\p@}{\Bheadsize\mediumseriffont\AJW@hangraggedright}}  %%EngD changed
 \def\subsubsection{\@startsection{subsubsection}{3}{\z@}
  {-13\p@ \@plus-7\p@ \@minus-3.5\p@}{.00001\p@}{\Cheadsize\bfseries\AJW@hangraggedright}}  %%EngD changed
 \def\paragraph{\@startsection{paragraph}{4}{\z@}
  {-13\p@ \@plus-7\p@ \@minus-3.5\p@}{.00001\p@}{\Cheadsize\itshape\AJW@raggedright}}  %%EngD changed
 \def\subparagraph{\@startsection{subparagraph}{5}{\z@}
  {13\p@ \@plus7\p@ \@minus3.5\p@}{-0.5em}{\normalsize\itshape\AJW@raggedright}}
%
 \newcommand\xhead{\@startsection{xhead}{6}{\z@}%
   {-6.5pt plus-1pt minus-1pt}{3\p@}%
   {\normalsize\mediumseriffont\AJW@raggedright
   }}
 \newcounter{xhead}
 \newcommand\xheadmark[1]{}
%
 \newcommand\problemhead{\@startsection{problemhead}{6}{-17mm}%  %%EngD changed
   {-12.5pt plus-1pt minus-1pt}{12\p@}%  %%EngD changed
   {\normalsize\boldseriffont\AJW@raggedright
   }}
 \newcounter{problemhead}
 \newcommand\problemheadmark[1]{}


\def\AJW@hangraggedright{\rightskip\@flushglue \let\\=\newline}
\def\AJW@raggedright{\rightskip\@flushglue \let\\=\@centercr
 \parindent=\AJW@indent}
%
% Default initializations of \...mark commands
%
%\def\chaptermark#1{}
\newcommand*\chaptermark[1]{}
\setcounter{secnumdepth}{2}

%
% APPENDIX
%
\ifAJW@multisty
%
  \def\appendix{%
    \apptmp=\c@chapter %save chapter number
    \par
    \AJW@appendixtrue
    \def\section{\@startsection {section}{1}{\z@}
       {-26\p@ \@plus-7\p@ \@minus-3.5\p@}{13\p@}{\Aheadsize\boldseriffont\AJW@hangraggedright Appendix~}}
    \setcounter{section}{0}
    \@addtoreset{figure}{subsection}
    \setcounter{figure}{0}
    \setcounter{table}{0}
% set tocdepth to chapter heads only whether multi-author or single-author
    %\addtocontents{toc}{\protect\setcounter{tocdepth}{1}}
    \def\@chapapp{\appendixname}
    \def\thechapter {\Alph{chapter}}
    \def\thesection {\Alph{section}}
    \def\thesubsection {\Alph{section}.\arabic{subsection}}
    \def\thetable   {\Alph{section}.\@arabic\c@table}
    \def\thefigure  {\Alph{section}.\@arabic\c@figure}
    \def\theequation{\Alph{section}.\arabic{equation}}
}

% and only one appendix
  \def\oneappendix{\AJW@oneappendixtrue\appendix
    \def\thesection {}% remove the 'A' from Appendix heading
  }
\else
%
% single version
\def\appendix{\par\clearpage
 \AJW@appendixtrue
 \setcounter{chapter}{0}
 \setcounter{section}{0}
% set tocdepth to chapter heads only whether multi-author or single-author
 \addtocontents{toc}{\protect\setcounter{tocdepth}{0}}
 \def\@chapapp{\appendixname}
 \def\thechapter {\Alph{chapter}}
 \def\thesection {\thechapter.\arabic{section}}
 \def\thetable   {\thechapter.\@arabic\c@table}
 \def\thefigure  {\thechapter.\@arabic\c@figure}
 \def\theequation{\thechapter.\arabic{equation}}
}
%
% and only one appendix
    \def\oneappendix{\AJW@oneappendixtrue\appendix}
\fi


%%%%%%%%%%%%%%%%%%%%%%
\ifAJW@multisty
  \def\endappendix{\AJW@appendixfalse
  \def\section{\@startsection {section}{1}{\z@}
    {-26\p@ \@plus-7\p@ \@minus-3.5\p@}{13\p@}{\Aheadsize\boldseriffont\AJW@hangraggedright}}
    \def\thechapter {\arabic{chapter}}
    \def\thesection {\thechapter.\arabic{section}}
    \def\thesubsection {\thesection.\arabic{subsection}}
    \def\thetable{\thechapter.\@arabic\c@table}
    \def\thefigure{\thechapter.\@arabic\c@figure}
    \def\theequation{\arabic{chapter}.\arabic{equation}}%
  }
\else
  \def\endappendix{\relax}
\fi

%    ****************************************
%    *                LISTS                 *
%    ****************************************
%
\ifAJW@indentsty
 \ifdim \AJW@indent=\z@ \setlength\leftmargini{1em}
  \else \setlength\leftmargini{\AJW@indent}
 \fi \else \setlength\leftmargini{1em}
\fi
\setlength\leftmarginii{1em}
\setlength\leftmarginiii{1em}
\setlength\leftmarginiv{1em}
\setlength\leftmarginv{1em}
\setlength\leftmarginvi{1em}
\leftmargin\leftmargini
\setlength\labelsep{0.5em}
\labelwidth\leftmargini \advance\labelwidth-\labelsep
\def\@listI{\leftmargin\leftmargini
 \parsep    \z@ \@plus 2\p@
 \partopsep \z@ \@plus 1\p@
 \topsep  6.5\p@ \@plus 3.25\p@ \@minus 1\p@%
 \itemsep   \z@ \@plus.1\p@
 \listparindent \parindent
}
\let\@listi\@listI
\@listi
\def\@listii{\leftmargin\leftmarginii
 \labelwidth\leftmarginii \advance\labelwidth-\labelsep
 \parsep    \z@ \@plus 2\p@
 \partopsep \z@ \@plus 1\p@
%\topsep 3.25\p@ \@plus .5\p@ \@minus 1\p@
 \topsep    \z@ \@plus.1\p@
 \itemsep   \z@ \@plus.1\p@
}
\def\@listiii{\leftmargin\leftmarginiii
 \labelwidth\leftmarginiii \advance\labelwidth-\labelsep
 \parsep    \z@ \@plus 2\p@
 \partopsep \z@ \@plus 1\p@
%\topsep 3.25\p@ \@plus .5\p@ \@minus 1\p@%
 \topsep    \z@ \@plus.1\p@
 \itemsep   \z@ \@plus.1\p@
}
\def\@listiv{\leftmargin\leftmarginiv
 \labelwidth\leftmarginiv \advance\labelwidth-\labelsep}
\def\@listv{\leftmargin\leftmarginv
 \labelwidth\leftmarginv  \advance\labelwidth-\labelsep}
\def\@listvi{\leftmargin\leftmarginvi
 \labelwidth\leftmarginvi \advance\labelwidth-\labelsep}

%    ****************************************
%    *           LIST ENVIRONMENTS          *
%    ****************************************
%
%
% ENUMERATE -- with optional argument to set left margin
% DEFAULT is arabic numbering with full point

\def\makeRRlabel#1{\hss\llap{#1}}
\def\makeRRlabeldot#1{\hss\llap{#1.}}

\newbox\@dotbox
\setbox\@dotbox\hbox{.}%
\def\enumerate{\ifnum \@enumdepth >3 \@toodeep \else
  \advance\@enumdepth \@ne
  \edef\@enumctr{enum\romannumeral\the\@enumdepth}%
 \fi
 \@ifnextchar [{\@enumeratetwo}{\@enumerateone}%
}
\def\@enumeratetwo[#1]{%
 \list{\csname label\@enumctr\endcsname}%
  {\settowidth\labelwidth{#1}%
   \leftmargin\labelwidth \advance\leftmargin\labelsep
   \usecounter{\@enumctr}%
   \let\makelabel\makeRRlabel}%
}
\def\@enumerateone{%\listsize
 \list{\csname label\@enumctr\endcsname}%
  {\usecounter{\@enumctr}%
   \ifAJW@indentsty
    \ifdim \AJW@indent=\z@ \leftmargin 1em % overrides \leftmargini,ii,iii...
                 \fi \else %\leftmargin 1em
                 \advance\labelsep-0.5pt  %%EngD changed
                           \leftmargin 0.5em% width of a digit
                           \advance\leftmargin\labelsep
                           \advance\leftmargin\wd\@dotbox
   \fi
   \let\makelabel\makeRRlabeldot}}

\newcommand\labelenumi{\theenumi}
\newcommand\labelenumii{\theenumii}
\newcommand\labelenumiii{\theenumiii}
\newcommand\labelenumiv{\theenumiv}

\renewcommand\p@enumii{\theenumi}
\renewcommand\p@enumiii{\theenumi(\theenumii)}
\renewcommand\p@enumiv{\p@enumiii\theenumiii}
%
% ITEMIZE
%
\def\itemize{\ifnum \@itemdepth >3 \@toodeep \else
  \advance\@itemdepth \@ne
  \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
%  \listsize
  \setlength\leftmargini{1em}%
  \list{\csname\@itemitem\endcsname}%
  {\def\makelabel##1{\hbox to 1pc{##1\hfill}}
   \labelsep -2.8pt%  %%EngD changed from 0pt
   \itemindent -2pc%
   \labelwidth 0pt%
   \leftmargin 1.74pc%  %%EngD changed from 2pc
   }%
 \fi
}
\newcommand\labelitemi{$\bullet$}
\newcommand\labelitemii{\bfseries --}
\newcommand\labelitemiii{$\circ$}
\newcommand\labelitemiv{$\diamond$}
%
% DESCRIPTION
%
\newcommand*\descriptionlabel[1]{\hspace\labelsep\bfseries #1}
\def\description{%\listsize
 \list{}{\labelwidth\z@
  \leftmargin 3pc
  \itemindent-\leftmargin
  \let\makelabel\descriptionlabel
 }}
\let\enddescription\endlist
%

%    ****************************************
%    *         OTHER ENVIRONMENTS           *
%    ****************************************
%
% VERSE
%
\def\verse{\let\\=\@centercr
  \list{}{\itemsep\z@ \itemindent -1em\listparindent \itemindent
          \rightmargin\z@
          \leftmargin\AJW@indent\advance\leftmargin 1em}\item[]\smallish}
\let\endverse=\endlist
%
% QUOTATION
%
\def\quotation{\list{}{\leftmargin\AJW@indent\rightmargin\z@}\item[]\smallish}
\let\endquotation=\endlist
%
% QUOTE
%
\def\quote{\list{}{\leftmargin\AJW@indent \rightmargin\z@}\item[]\smallish}
\let\endquote=\endlist
%
% INDENTED (for use in title pages)
%
\def\indented{\list{}{\leftmargin\AJW@indent
 \listparindent\z@\rightmargin\z@}\raggedright\item[]}
\let\endindented=\endlist
%
% ARRAY AND TABULAR
%
\setlength\arraycolsep{5\p@}
\setlength\tabcolsep{6\p@}
\setlength\arrayrulewidth{.5\p@}
\setlength\doublerulesep{2\p@}
%
% TABBING
%
\tabbingsep\labelsep
%
% FRAMEBOX
%
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.5\p@}


% THEOREM
%
% let theorems be handled by amsthm.sty


% PROOFs
%
% let proofs be handled by amsthm.sty

\newenvironment{proof*}[1][\proofname]{\par
  \pushQED{}%{\qed}%
  %\normalfont
  \topsep6\p@\@plus6\p@\relax
  \trivlist
  \item[\hskip\labelsep
        \itshape
    #1\@addpunct{\enskip}]\ignorespaces
}{%
  \popQED\endtrivlist\@endpefalse
}


%    ****************************************
%    *        CHAPTERS AND SECTIONS         *
%    ****************************************
%
% DEFINE COUNTERS:
%
\newcounter {part}
\newcounter {chapter}
\newcounter {section}[chapter]
\newcounter {subsection}[section]
\newcounter {subsubsection}[subsection]
\newcounter {paragraph}[subsubsection]
\newcounter {subparagraph}[paragraph]

\def\thepart          {\Roman{part}}
\def\thechapter       {\arabic{chapter}}
\def\thesection       {\thechapter.\arabic{section}}
\def\thesubsection    {\thesection.\arabic{subsection}}
\def\thesubsubsection {\thesubsection .\arabic{subsubsection}}
\def\theparagraph     {\thesubsubsection.\arabic{paragraph}}
\def\thesubparagraph  {\theparagraph.\arabic{subparagraph}}
\def\@chapapp{\chaptername}

%    ****************************************
%    *                 PARTS                *
%    ****************************************
%

\newcommand\ls{\kern.1em} % for letterspacing; .15em is a hair less than thinspace
\newcommand\ns{\kern.4em} % for name-spacing

\def\part{\cleardoublepage
 \thispagestyle{empty}
 \if@twocolumn
   \onecolumn
   \@tempswatrue
  \else \@tempswafalse
 \fi
 \secdef\@part\@spart
}

% rule appears in Part title whether [multi] option is used or not
\def\@part[#1]#2{\ifnum \c@secnumdepth >-2\relax
  \refstepcounter{part}%
  \ifAJW@indentsty
   \ifdim\AJW@indent>\z@
    \ifAJW@multisty
     \addcontentsline{toc}{part}{{\partname\ \thepart\quad #1}}%
    \else
     \addcontentsline{toc}{part}{{\partname\ \thepart\quad #1}}%
    \fi
   \else
     \addcontentsline{toc}{part}{{\partname\ \thepart\quad #1}}%
   \fi
  \else
    \addcontentsline{toc}{part}{{\partname\ \thepart\quad #1}}%
  \fi
  \else
    \addcontentsline{toc}{part}{#1}%
  \fi
  \@mkboth{}{}%
  {\raggedright
  %\vspace*{-1.4\baselineskip}
  \vspace*{-5.1mm}  %%EngD changed
  \ifnum \c@secnumdepth >-2\relax
    \hspace*{-17mm}{\huge\boldseriffont{\partname~\thepart}}\par  %%EngD changed
    \vskip -4.6mm%  %%EngD changed
      \@insertgreyrunningheadrule
    \vskip 19pt  %%EngD changed
   \else
    \par
    \vskip 2.5\baselineskip
  \fi
  \parttitlesize\boldseriffont {#2}\par}
  \@endpart
}

% sPart titles not covered
\def\@spart#1{{\ifAJW@indentsty \raggedright \else \centering \fi
  \vspace*{-\baselineskip}
  \par
% \vskip 19.5\p@
  \vskip 13\p@
      %\ifsp@nningrule
         \vskip 0\p@
         \rule{\textwidth}{0.4pt}\par
         \vskip 7pt
      %\else
      %  \vskip 20\p@% part title on 4th line
      %\fi
  \huge \rm \MakeUppercase{#1}\par}
\@endpart}
%
% \@endpart finishes the part page
%
\def\@endpart{\cleardoublepage}


%    ****************************************
%    *                EXAMPLES              *
%    ****************************************


% Examples numbered by chapter
\newcounter{example@bean}[chapter]
\def\p@example@bean{\thechapter.}% add section number to cross-references

\newcount\tmp
\def\examplelist{\@ifnextchar [{\@examplelisttwo}{\@examplelistone}}
% use \@exampleliston - heading 'Example'

\newcommand\solution{\vspace{6.5pt}\par\noindent{\mediumseriffont Solution}\\*}

\def\@examplelistone{\ifnum \@enumdepth >3 \@toodeep \else
  \advance\@enumdepth \@ne
  \def\examplelabel##1{\boldseriffont Example \thechapter.##1\hss}
    \vspace{\baselineskip}%
    \par\noindent\rule{\textwidth}{1pt}%
    \par\nobreak
    \@afterheading
     \list{%
       \arabic{example@bean}}%
       {\tmp=\c@example@bean
        \usecounter{example@bean}%
        \c@example@bean=\tmp
        \leftmargin 0pc%
        \labelwidth \leftmargin
        \labelsep 1em%ajw
        \addtolength{\labelwidth}{-\labelsep}%
        \itemsep\baselineskip
        \partopsep=0pt plus0pt minus0pt%
        \raggedright%  %%EngD added
        \let\makelabel\examplelabel}%
  \fi}

\def\endexamplelist{\\*[-4pt]\noindent\rule{\textwidth}{1pt}\endlist\addvspace{17pt}}


%\def\@examplelisttwo[#1]{\ifnum \@enumdepth >3 \@toodeep \else
%  \advance\@enumdepth \@ne
%  \def\examplelabel##1{\bf #1 \thechapter.##1\hss}
%  \list{\arabic{example@bean}}%
%       {\tmp=\c@example@bean%new line
%        \usecounter{example@bean}%
%        \c@example@bean=\tmp%new line
%        \leftmargin 2pc
%        \labelwidth \leftmargin
%        \labelsep 1em%ajw
%        \addtolength{\labelwidth}{-\labelsep}
%  \let\makelabel\examplelabel}%
%\fi}

%\let\endexamplelist=\endlist


% Examples numbered by chapter
%\newcounter{example@beanbychapter}[chapter]
%\def\p@example@beanbychapter{\thechapter.}% add chapter number to cross-references

%\def\examplelistbychapter{\@ifnextchar [{\@examplelisttwobychapter}{\@examplelistonebychapter}}
%\def\@examplelistonebychapter{\ifnum \@enumdepth >3 \@toodeep \else
%  \advance\@enumdepth \@ne
%  \def\examplelabel##1{\rm \thechapter.##1\hss}
%  \list{\arabic{example@beanbychapter}}%
%       {\usecounter{example@beanbychapter}%
%        \leftmargin 2pc
%        \labelwidth \leftmargin
%        \addtolength{\labelwidth}{-\labelsep}
%  \let\makelabel\examplelabel}%
%\fi}
%\def\@examplelisttwobychapter[#1]{\ifnum \@enumdepth >3 \@toodeep \else
%  \advance\@enumdepth \@ne
%  \def\examplelabel##1{\bf #1 \thechapter.##1\hss}
%  \list{\arabic{example@beanbychapter}}%
%      {\tmp=\c@example@beanbychapter%new line
%       \usecounter{example@beanbychapter}%
%        \c@example@beanbychapter=\tmp%new line
%        \leftmargin 2pc
%        \labelwidth \leftmargin
%        \addtolength{\labelwidth}{-\labelsep}
%  \let\makelabel\examplelabel}%
%\fi}

%\let\endexamplelistbychapter=\endlist

%\def\examples{\section*{Examples}%
%  \addcontentsline{toc}{section}{Examples}
%  \ifAJW@multisty\else\markright{Examples}\fi
%  \examplelistbychapter
%  %\setcounter{equation}{0}
%  %\def\theequation{E\thechapter.\arabic{equation}}
%}
%\let\endexamples=\endexamplelistbychapter
%\let\example=\item


%    ****************************************
%    *               PROBLEMS              *
%    ****************************************

% Problems numbered by chapter
\newcounter{problem@bean}[chapter]
\def\p@problem@bean{\thechapter.}% add chapter number to cross-references

\newcount\tmp
\def\problemlist{%\@ifnextchar [{\@problemlisttwo}
   \AJW@addtocfalse
     \problemhead{Problems}
   \AJW@addtoctrue
  \@problemlistone}
% use \@problemlistone - heading always blank

\def\@problemlistone{\ifnum \@enumdepth >3 \@toodeep \else
  \advance\@enumdepth \@ne
  \def\problemlabel##1{\boldseriffont\thechapter.##1\hss}
  \list{\arabic{problem@bean}}%
       {\tmp=\c@problem@bean%new line
        \usecounter{problem@bean}%
        \c@problem@bean=\tmp%new line
        \leftmargin 0pc
        \labelwidth \leftmargin
        \labelsep 1em%ajw
        \addtolength{\labelwidth}{-\labelsep}
  \let\makelabel\problemlabel}%
\fi}

\let\endproblemlist=\endlist

%\def\@problemlisttwo[#1]{\ifnum \@enumdepth >3 \@toodeep \else
%  \advance\@enumdepth \@ne
%  \def\problemlabel##1{\bf #1 \thesection.##1\hss}
%  \list{\arabic{problem@bean}}%
%       {\tmp=\c@problem@bean%new line
%        \usecounter{problem@bean}%
%        \c@problem@bean=\tmp%new line
%       \leftmargin 2pc
%        \labelwidth \leftmargin
%        \labelsep 1em%ajw
%        \addtolength{\labelwidth}{-\labelsep}
%  \let\makelabel\problemlabel}%
%\fi}

% Problems numbered by chapter
%\newcounter{problem@beanbychapter}[chapter]
%\def\p@problem@beanbychapter{\thechapter.}% add chapter number to cross-references

%\def\problemlistbychapter{\@ifnextchar [{\@problemlisttwobychapter}{\@problemlistonebychapter}}
%\def\@problemlistonebychapter{\ifnum \@enumdepth >3 \@toodeep \else
%  \advance\@enumdepth \@ne
%  \def\problemlabel##1{\rm \thechapter.##1\hss}
%  \list{\arabic{problem@beanbychapter}}%
%       {\usecounter{problem@beanbychapter}%
%        \leftmargin 2pc
%        \labelwidth \leftmargin
%        \addtolength{\labelwidth}{-\labelsep}
%  \let\makelabel\problemlabel}%
%\fi}
%\def\@problemlisttwobychapter[#1]{\ifnum \@enumdepth >3 \@toodeep \else
%  \advance\@enumdepth \@ne
%  \def\problemlabel##1{\bf #1 \thechapter.##1\hss}
%  \list{\arabic{problem@beanbychapter}}%
%       {\tmp=\c@problem@beanbychapter%new line
%        \usecounter{problem@beanbychapter}%
%        \c@problem@beanbychapter=\tmp%new line
%        \leftmargin 2pc
%        \labelwidth \leftmargin
%        \addtolength{\labelwidth}{-\labelsep}
%  \let\makelabel\problemlabel}%
%\fi}

%\let\endproblemlistbychapter=\endlist

%\def\problems{\section*{Problems}%
%  %\addcontentsline{toc}{section}{Problems}
%  \ifAJW@multisty\else\markright{Problems}\fi
%  \problemlistbychapter
%  %\setcounter{equation}{0}
%  %\def\theequation{E\thechapter.\arabic{equation}}
%}
%\let\endproblems=\endproblemlistbychapter
%\let\problem=\item


%    ****************************************
%    *         TABLE OF CONTENTS, ETC.      *
%    ****************************************
%
\newdimen\AJW@tocindent
  \AJW@tocindent=45\p@ % used for Appendix entries   %%EngD changed from 60
  \def\numberline#1{\hbox to\@tempdima{#1\hfil}}

\def\AJW@numberline#1{\hbox to 1.15\AJW@tocindent {{\appendixname\ #1\hfil}}}%1.15
\def\AJW@nonumberline#1{\hbox to 56pt{{\appendixname\hfil}}}%56pt matches section heads

%\ifAJW@indentsty \ifdim\AJW@indent>\z@
% \def\numberline#1{\hbox to\@tempdima{\hfil#1\hspace*{0.5em}}}
% \def\AJW@numberline#1{\hspace*{\@tempdima}%
%  \hbox to 1.15\AJW@tocindent {{\itshape \appendixname\ #1\hfil}}}
%\fi \fi
%
\newif\ifAJW@addtoc        \AJW@addtocfalse
\newif\ifAJW@firstTOCentry \AJW@firstTOCentrytrue
\newdimen\AJW@toclmarg
%\newdimen\AJW@tocfloatlmarg
\ifAJW@multisty
  \setcounter{tocdepth}{0}
  \AJW@toclmarg 17mm  %%EngD changed from 4pc
% \AJW@tocfloatlmarg 16\p@
\else
  \setcounter{tocdepth}{1}
  \AJW@toclmarg 17mm%  %%EngD changed from 4pc
% \AJW@tocfloatlmarg 28\p@
\fi

\ifAJW@indentsty %and
  \ifdim\AJW@indent>\z@
    \AJW@toclmarg \AJW@indent
%   \AJW@tocfloatlmarg \AJW@indent
  \fi
\fi
\def\@pnumwidth{2.55em}%{1.55em}
\def\@tocrmarg {3.55em}%{2.55em}
\def\@dotsep{4.5}
%
% TABLEOFCONTENTS
%
\newcommand\tableofcontents{\@restonecolfalse
 \if@twocolumn\@restonecoltrue\onecolumn\fi
 \AJW@addtocfalse
 \chapter*{\contentsname}
 \vskip-0.3pt  %%EngD added
 \@starttoc{toc}
 \AJW@addtoctrue
 \if@restonecol\twocolumn\fi
}

\newcommand*\l@part[2]{\vspace{\baselineskip}%
 \addpenalty{-\@highpenalty}%
 \begingroup
  \parindent 0pc%  %%EngD changed from -4pc
  \rightskip \@pnumwidth
  \parfillskip -\@pnumwidth\leftskip-17mm  %%EngD changed from 4pc
  \leavevmode
  %\ifAJW@multisty \else \hspace*{\AJW@toclmarg}\fi
  \ifAJW@firstTOCentry
    {\normalfont\large\boldseriffont #1} \hfil \hbox to\@pnumwidth{\hss {\normalfont\itshape page\/} #2}\par
    \global\AJW@firstTOCentryfalse
  \else
    {\normalfont\large\boldseriffont #1} \hfil \hbox to\@pnumwidth{\hss #2}\par
  \fi
  \nobreak
  \endgroup
}

\newcommand*\l@chapter[2]{\pagebreak[3]
 \vspace{\baselineskip}%
 \@tempdima \AJW@toclmarg
 \begingroup
  \parindent-17mm%\z@  %%EngD changed from -4pc
    \rightskip \@pnumwidth  \parfillskip -\@pnumwidth
  \leavevmode
  \advance \leftskip \@tempdima  \hskip -\leftskip%\advance\leftskip-48pt%  %%EngD changed
  \ifAJW@firstTOCentry
   {\normalfont\boldseriffont #1}\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss {\normalfont\itshape page\/} #2}\par\vskip4.1pt%  %%EngD changed
   \global\AJW@firstTOCentryfalse
  \else
   {\normalfont\boldseriffont #1}\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss #2}\par\vskip0.21pt%  %%EngD changed
  \fi
 \endgroup
}

\newcommand*\l@schapter[2]{\pagebreak[3]% now indented
 \@tempdima \z@
 \begingroup
  \parindent \z@
    \rightskip \@pnumwidth  \parfillskip -\@pnumwidth
  \leavevmode
  \advance \leftskip \@tempdima  \hskip -\leftskip
  \ifAJW@firstTOCentry
   {\itshape #1}\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss {\itshape page\/} #2}\par
   \global\AJW@firstTOCentryfalse
  \else
   {\itshape #1}\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss #2}\par
  \fi
 \endgroup
}
%%EngD changes begin here
\newcommand*\l@figure{\@dottedtocline{1}{0pc}{1.9pc}}% same as sections
\newcommand*\l@table{\@dottedtocline{1}{0pc}{1.9pc}}% same as sections

\newcommand*\l@author[2]{\pagebreak[3]
 \vspace{12.9pt}%
 \@tempdima \AJW@toclmarg
 \begingroup
  \parindent 0pc%\z@
    \rightskip \@pnumwidth  \parfillskip -\@pnumwidth
  \leavevmode
  \advance \leftskip \@tempdima  \hskip -\leftskip
   {\normalfont\lightseriffont\fontsize{9}{12}\selectfont\leftskip0pt #1}\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss}\par
 \endgroup
}
%
\def\tocsubsecfnt{\fontsize{9}{12}\selectfont}
\newbox\tocbox
\newdimen\tocsecnumwidth
\newdimen\tocsubsecnumwidth
\newdimen\tocsubsubsecnumwidth
\tocsecnumwidth=22\p@
\tocsubsecnumwidth=29.5\p@
\tocsubsubsecnumwidth=35\p@
%
\def\tocmax#1#2#3{\tocsecmax{#1}\tocsubsecmax{#2}\tocsubsubsecmax{#3}}
%
\def\tocsecmax#1{\setbox\tocbox\hbox{#1\hskip0.98em}\global\tocsecnumwidth=\wd\tocbox}
\def\tocsubsecmax#1{\setbox\tocbox\hbox{#1\hskip0.98em}\global\tocsubsecnumwidth=\wd\tocbox}%\global\advance\tocsubsecnumwidth\tocsecnumwidth
\def\tocsubsubsecmax#1{\setbox\tocbox\hbox{#1\hskip0.98em}\global\tocsubsubsecnumwidth=\wd\tocbox}%\global\advance\tocsubsubsecnumwidth\tocsubsecnumwidth
%
\newcommand*\l@section      {\@dottedtocline{1}{0pt}{\tocsecnumwidth}}
\newcommand*\l@subsection   {\@subdottedtocline{2}{\tocsecnumwidth}{\tocsubsecnumwidth}}
%
\def\@dottedtocline#1#2#3#4#5{% For toc alone, all designs assume \AJW@indent=0pc
 \ifnum #1>\c@tocdepth
 \else
  \vskip \z@%
  {\leftskip #2\relax
   \rightskip \@tocrmarg \@plus2em % for ragged right entries
   \parfillskip -\rightskip
   \parindent #2\relax
   \@afterindenttrue
   \interlinepenalty\@M
   \leavevmode
   \@tempdima #3\relax \advance\leftskip \@tempdima \hbox{}\hskip -\leftskip
   #4\nobreak%\ifnum#1=2\tocsubsecfnt\fi
   \hfill \nobreak
   \ifAJW@firstTOCentry
    \hbox to\@pnumwidth{\hfil\rm {\itshape page\/} #5}\par
    \global\AJW@firstTOCentryfalse
   \else
    \hbox to\@pnumwidth{\hfil\rm #5}\par
   \fi}%
 \fi
}

\def\@subdottedtocline#1#2#3#4#5{% For toc alone, all designs assume \AJW@indent=0pc
 \ifnum #1>\c@tocdepth
 \else
  \vskip \z@%
  {\leftskip #2\relax
   %\rightskip \@tocrmarg \@plus2em % for ragged right entries
   \rightskip5.8pc plus1fill{} % for ragged right entries
   \parfillskip -\rightskip
   \parindent #2\relax
   \@afterindenttrue
   \interlinepenalty\@M
   \leavevmode
   \@tempdima #3\relax \advance\leftskip \@tempdima \hbox{}\hskip -\leftskip
   #4\nobreak%\ifnum#1=2\tocsubsecfnt\fi
   \hfill \nobreak
   \ifAJW@firstTOCentry
    \hbox to\@pnumwidth{\hfil\rm {\itshape page\/} #5}\par
    \global\AJW@firstTOCentryfalse
   \else
    \hbox to\@pnumwidth{\hfil\rm #5}\par
   \fi}%
 \fi
}

%\newcommand*\l@part[2]{\vspace{\baselineskip}%
% \addpenalty{-\@highpenalty}%
% \begingroup
%  \parindent -17mm%
%  \rightskip \@pnumwidth
%  \parfillskip -\@pnumwidth
%  \leavevmode
%  %\ifAJW@multisty \else \hspace*{\AJW@toclmarg}\fi
%  \ifAJW@firstTOCentry
%    {\large\boldseriffont #1} \hfil \hbox to\@pnumwidth{\hss {\normalfont\itshape page\/} #2}\par
%    \global\AJW@firstTOCentryfalse
%  \else
%    {\large\boldseriffont #1} \hfil \hbox to\@pnumwidth{\hss #2}\par
%  \fi
%  \nobreak
%  \endgroup
%}
%
%\newcommand*\l@chapter[2]{\pagebreak[3]
% \vspace{\baselineskip}%
% \@tempdima \AJW@toclmarg
% \begingroup
%  \parindent -17mm%\z@
%    \rightskip \@pnumwidth  \parfillskip -\@pnumwidth
%  \leavevmode
%  \advance \leftskip \@tempdima  \hskip -\leftskip
%  \ifAJW@firstTOCentry
%   {\normalfont\boldseriffont #1}\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss {\normalfont\itshape page\/} #2}\par
%   \global\AJW@firstTOCentryfalse
%  \else
%   {\normalfont\boldseriffont #1}\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss #2}\par
%  \fi
% \endgroup
%}
%
%\newcommand*\l@schapter[2]{\pagebreak[3]% now indented
% \@tempdima \z@
% \begingroup
%  \parindent \z@
%    \rightskip \@pnumwidth  \parfillskip -\@pnumwidth
%  \leavevmode
%  \advance \leftskip \@tempdima  \hskip -\leftskip
%  \ifAJW@firstTOCentry
%   {\itshape #1}\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss {\itshape page\/} #2}\par
%   \global\AJW@firstTOCentryfalse
%  \else
%   {\itshape #1}\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss #2}\par
%  \fi
% \endgroup
%}
%
%\newcommand*\l@figure{\@dottedtocline{1}{-17mm}{17mm}}% same as sections


%%EngD changes end here
% remove page number from contributor page
\newcommand*\l@contrib[2]{\pagebreak[3]
 \@tempdima \z@%\AJW@toclmarg
 \begingroup
% \parindent \z@  \rightskip \@pnumwidth  \parfillskip -\@pnumwidth
  \parindent \z@  \raggedright
  \leavevmode
  \advance \leftskip \@tempdima  \hskip -\leftskip
  \ifAJW@firstTOCentry
   {#1}\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss {\itshape page\/} #2}\par
   \global\AJW@firstTOCentryfalse
  \else
%  {#1}\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss #2}\par
   {#1}\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss }\par
  \fi
 \endgroup
}

% (sections not listed in multi-author designs)

%\newcommand*\l@section      {\@dottedtocline{1}{0pt}{28pt}} %%EngD deleted
%\newcommand*\l@subsection   {\@dottedtocline{2}{28pt}{35pt}}  %%EngD deleted
\newcommand*\l@problemhead  {\@dottedtocline{6}{0pt}{28pt}}

% these are not used
\newcommand*\l@subsubsection{\@dottedtocline{3}{\z@}{\AJW@toclmarg}}
\newcommand*\l@paragraph    {\@dottedtocline{4}{\z@}{\AJW@toclmarg}}
\newcommand*\l@subparagraph {\@dottedtocline{5}{\z@}{\AJW@toclmarg}}
\newcommand*\l@xhead        {\@dottedtocline{6}{1.5em}{2.3em}}
 %
% LIST OF FIGURES
%
\newcommand\listoffigures{\@restonecolfalse
 \if@twocolumn\@restonecoltrue\onecolumn\fi
 \bgroup
 \AJW@addtocfalse% different chapter title and toc entries
   \chapter*{\listfigurename}
   \addcontentsline{toc}{schapter}{{List of illustrations}}
 \normalsize
 \setcounter{tocdepth}{1}
 \@starttoc{lof}
 \egroup
 \if@restonecol\twocolumn\fi
}

%
% LIST OF TABLES
%
\newcommand\listoftables{\@restonecolfalse
 \if@twocolumn\@restonecoltrue\onecolumn\fi
 \bgroup
 \AJW@addtocfalse% different chapter title and toc entries
   \chapter*{\listtablename}
   \addcontentsline{toc}{schapter}{List of tables}
 \normalsize
 \setcounter{tocdepth}{1}
 \@starttoc{lot}
 \egroup
 \if@restonecol\twocolumn\fi
}

\let\l@table\l@figure

% LIST OF FLOATINGBOXES
%
\newcommand\listoffloatingboxes{\@restonecolfalse
 \if@twocolumn\@restonecoltrue\onecolumn\fi
 \bgroup
 \AJW@addtocfalse% different chapter title and toc entries
   \chapter*{\listfloatingboxes}
   \addcontentsline{toc}{schapter}{List of boxes}
 \normalsize
 \setcounter{tocdepth}{1}
 \@starttoc{lofb}
 \egroup
 \if@restonecol\twocolumn\fi
}

\let\l@floatingbox\l@figure

% LIST OF CONTRIBUTORS
\newcommand\contributor[1]{%
  \addcontentsline{loc}{contrib}{#1}%
  \addtocontents{loc}{\vspace{\baselineskip}}%
}
\newcommand\affiliation[1]{\\*{\normalfont{#1}}}

\newcommand\listofcontributors{\@restonecolfalse
 \if@twocolumn\@restonecoltrue\onecolumn\fi
 \bgroup
 \AJW@addtocfalse% different chapter title and toc entries
   \chapter*{\listcontribname}
   \addcontentsline{toc}{schapter}{List of contributors}
 \normalsize\boldseriffont
 \setcounter{tocdepth}{1}
 \@starttoc{loc}
 \egroup
 \if@restonecol\twocolumn\fi
}

\newcommand\editedlistofcontributors{\@restonecolfalse
 \if@twocolumn\@restonecoltrue\onecolumn\fi
 \bgroup
 \AJW@addtocfalse% different chapter title and toc entries
   \chapter*{\listcontribname}
   \addcontentsline{toc}{schapter}{List of contributors}
 \normalsize
 \setcounter{tocdepth}{1}
 %\@starttoc{loc}
 \input{editedloc}
 \egroup
 \if@restonecol\twocolumn\fi
}

%
% \@dottedtocline{LEVEL}{INDENT}{NUMWIDTH}{TITLE}{PAGE} without the dots
%

\def\@dottedtocline#1#2#3#4#5{% For toc alone, all designs assume \AJW@indent=0pc
 \ifnum #1>\c@tocdepth
 \else
  \vskip \z@ \@plus .2\p@
  {\leftskip #2\relax
   \rightskip \@tocrmarg \@plus2em % for ragged right entries
   \parfillskip -\rightskip
   \parindent #2\relax
   \@afterindenttrue
   \interlinepenalty\@M
   \leavevmode
   \@tempdima #3\relax \advance\leftskip \@tempdima \hbox{}\hskip -\leftskip
   #4\nobreak
   \hfill \nobreak
   \ifAJW@firstTOCentry
    \hbox to\@pnumwidth{\hfil\rm {\itshape page\/} #5}\par
    \global\AJW@firstTOCentryfalse
   \else
    \hbox to\@pnumwidth{\hfil\rm #5}\par
   \fi}%
 \fi
}

%    ****************************************
%    *             BIBLIOGRAPHY             *
%    ****************************************
%

% default bibliography styles (use of harvard.sty will overwrite this coding)

\newenvironment{thebibliography}[1]
     {%
     \ifAJW@multisty
       \section*{\refname}%
     \else
       \chapter*{\refname}%
     \fi
     \@mkboth{\refname}{\refname}%
     \list{\@biblabel{\@arabic\c@enumiv}}%
          {\bibliosize
           \settowidth\labelwidth{\@biblabel{#1}}%
           \leftmargin\labelwidth
           \advance\leftmargin\labelsep
           \@openbib@code
           \usecounter{enumiv}%
           \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
     \sloppy
     \clubpenalty4000
     \@clubpenalty \clubpenalty
     \widowpenalty4000%
     \sfcode`\.\@m}
    {\def\@noitemerr
      {\@latex@warning{Empty `thebibliography' environment}}%
     \endlist}

\newcommand\newblock{\hskip .11em\@plus.33em\@minus.07em}
\let\@openbib@code\@empty

% HARVARD OUTPUT
%
\newcommand*\harvardoutputlabel[1]{}
\def\harvardoutput{\bibliosize
 \list{}{\labelwidth\z@
  \leftmargin 1em%
  \itemindent-\leftmargin
  \let\makelabel\harvardoutputlabel
 }}
\let\endharvardoutput\endlist

% VANCOUVER OUTPUT

% to remove square braces around references, i.e. 1 not [1]
\newcommand\removesquarebraces{%
  \renewcommand\@biblabel[1]{##1}%
}

\newenvironment{vancouveroutput}[1]
  {\bibliosize
   %\chapter*{\refname
   %\@mkboth{\refname}{\refname}}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

% IEEEtranoutput exactly the same as vancouveroutput
\newenvironment{IEEEtranoutput}[1]
  {\bibliosize
   %\chapter*{\refname
   %\@mkboth{\refname}{\refname}}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}


%    ****************************************
%    *              THE INDEX               *
%    ****************************************
%
% THE THEINDEX ENVIRONMENT
%

\def\index@prologue{}
\def\@indextype{}

\newif\if@restonecol
\newenvironment{theindex}{\the@index{\indexname}}{\par\endthe@index}
%\newenvironment{theauthorindex}{\the@index{Author index}}{\par\endthe@index}
%\newenvironment{thesubjectindex}{\the@index{Subject index}}{\par\endthe@index}

\newcommand\the@index[1]{\@restonecoltrue
 \if@twocolumn\@restonecolfalse\fi
 \columnseprule \z@
 \columnsep 1pc
 \twocolumn[\vspace*{10\p@}\mbox{}\par\@makeschapterhead{#1}]
 \@mkboth{#1}{#1}
 \def\leftmark{\indexname}%
 \def\rightmark{\indexname}%
 \typeout{#1.}
 \ifAJW@indentsty
  \ifdim\AJW@indent>\z@
   \addcontentsline{toc}{schapter}{\protect\numberline{\mbox{}}#1}
  \else
   \addcontentsline{toc}{schapter}{#1}
  \fi
 \else
  \addcontentsline{toc}{schapter}{#1}
 \fi
 \thispagestyle{plain}
 \indexsize\raggedright
 \parindent\z@
 \parskip\z@ \@plus.3\p@\relax
 \let\item=\@idxitem
}
\def\endthe@index{\if@restonecol\onecolumn\else\clearpage\fi}

\newcommand\@idxitem{\par\hangindent 2em}
\newcommand\subitem{\par\hangindent 3em \hspace*{1em}}
\newcommand\subsubitem{\par\hangindent 4em \hspace*{2em}}
%\newcommand\indexspace{\par \vskip 9\p@ \@plus 5\p@ \@minus 3\p@\relax}
\newcommand\indexspace{\par \vskip 3\p@ \@plus 1\p@ \@minus 1\p@\relax}


%    ****************************************
%    *          NUMBERED FOOTNOTES          *
%    ****************************************
%

\newskip\@footindent
\@footindent=1em

\renewcommand\footnoterule{\kern-3\p@ \hrule width 0\columnwidth \kern 2.6\p@}

% reset on chapters
\@addtoreset{footnote}{chapter}
 %%EngD next command changed
\AtBeginDocument{\long\def\@makefntext#1{\raggedright\@setpar{\@@par\@tempdima \hsize  
 \advance\@tempdima-\@footindent
 \parshape \@ne \@footindent \@tempdima}\par
 \noindent \kern-1.5\p@\hbox to \z@{\hss\@thefnmark\enskip}#1}}

\renewcommand\thefootnote{\mbox{$\fnsymbol{footnote}$}}
\def\@makefnmark{\hbox{\@thefnmark}}

% numbers preferred
%\def\fnsymbol#1{\expandafter\@fnsymbol\csname c@#1\endcsname}
 \def\@fnsymbol#1{{{^{\scriptstyle\@arabic{#1}}}\relax}}% correct


% redfine footnotemark and footnotetext to work on chapter openings
\newcommand\alphafootnotes{%
 \def\@fnsymbol##1{{{\scriptstyle\@alph{##1}}\relax}}%
 \def\@xfootnotenext[##1]{%
  \begingroup
     \csname c@\@mpfn\endcsname ##1\relax
     \unrestored@protected@xdef\@thefnmark{$^{\thempfn}$}%
  \endgroup
  \@footnotetext}%
}

\newcommand\arabicfootnotes{%
  \setcounter{footnote}{0}%
  \def\@fnsymbol##1{{{^{\scriptstyle\@arabic{##1}}}\relax}}%
}

%\def\@fnsymbol#1{\ifcase#1\or \dagger\or \ddagger\or
%   \S \or \P \or \|\or \dagger\dagger
%   \or \ddagger\ddagger\or \S\S\or \P\P\or \|\|\else *\fi\relax}
%
% MINIPAGE
%
\skip\@mpfootins = 4\p@ \@plus 1\p@ \@minus .1\p@  % top of first minipage footnote.
\def\thempfootnote{\alph{mpfootnote}}
\def\@makempfnmark{\hbox{$^{\@thefnmark}$}}
%
\long\def\@makempfntext#1{\@setpar{\@@par\@tempdima \hsize
 \advance\@tempdima-\@footindent
 \parshape \@ne \@footindent \@tempdima}\par
 \noindent \hbox to \z@{\hss$^{\@thefnmark}$\enskip}#1}
%
% redefine minipage to reset footnotemark as well as text and counters...
%
%\def\@iminipage[#1]#2{\leavevmode \@pboxswfalse
% \if #1b\vbox
%  \else \if #1t\vtop
%         \else \ifmmode \vcenter
%                \else \@pboxswtrue $\vcenter
%               \fi
%        \fi
% \fi\bgroup
% \hsize #2\textwidth\hsize \columnwidth\hsize
% \@parboxrestore
% \def\@mpfn{mpfootnote}
% \def\thempfn{\thempfootnote}\c@mpfootnote\z@
% \let\@makefnmark\@makempfnmark % v.01.03 AJW
% \let\@footnotetext\@mpfootnotetext
% \let\@listdepth\@mplistdepth \@mplistdepth\z@
% \@minipagerestore\global\@minipagetrue
% \everypar{\global\@minipagefalse\everypar{}}}

\def\@iiiminipage#1#2[#3]#4{% from latex.ltx
  \leavevmode
  \@pboxswfalse
  \setlength\@tempdima{#4}%
  \def\@mpargs{{#1}{#2}[#3]{#4}}%
  \setbox\@tempboxa\vbox\bgroup
    \color@begingroup
      \hsize\@tempdima
      \textwidth\hsize \columnwidth\hsize
      \@parboxrestore
      \def\@mpfn{mpfootnote}\def\thempfn{\thempfootnote}\c@mpfootnote\z@
      \let\@makefnmark\@makempfnmark % from v1.2 ajw
      \let\@footnotetext\@mpfootnotetext
      \let\@listdepth\@mplistdepth \@mplistdepth\z@
      \@minipagerestore\global\@minipagetrue %% \global added 24 May 89
      \everypar{\global\@minipagefalse\everypar{}}}

\long\def\@mpfootnotetext#1{\global\setbox\@mpfootins
 \vbox{\unvbox\@mpfootins
  \footnotesize
  \hsize\columnwidth \@parboxrestore
  \edef\@currentlabel{\csname p@mpfootnote\endcsname\@thefnmark}
  \@makempfntext{\rule{\z@}{\footnotesep}\ignorespaces #1\strut}}}

%       ****************************************
%       *               ENDNOTES               *
%       ****************************************
%
\newcounter{endnote}[chapter] \def\theendnote{\arabic{endnote}}
\newdimen\endnotesep
\newwrite\@enotes
\newif\if@enotesopen          \global\@enotesopenfalse
\newif\ifAJW@firstendnote     \global\AJW@firstendnotefalse

\def\@makeenmark{\hbox{$^{\@theenmark}$}}

\def\endnote{%
 \ifAJW@multisty \AJW@firstendnotefalse
        \else \ifnum \c@endnote = \z@ \AJW@firstendnotetrue
                                \else \AJW@firstendnotefalse \fi
 \fi
 \@ifnextchar[{\@xendnote
  }{\stepcounter{endnote}%
    \xdef\@theenmark{\theendnote}%
   }%
 \@endnotemark
 \@endnotetext
}
\def\@xendnote[#1]{\begingroup \c@endnote=#1\relax
  \xdef\@theenmark{\theendnote}\endgroup
}
\let\@doanenote=0
\let\@endanenote=0
\def\@openenotes{\immediate\openout\@enotes=\jobname.ent\relax
 \global\@enotesopentrue
}
\gdef\AJW@endnotehead{\relax}
\long\def\@endnotetext#1{%
 \if@enotesopen \else \@openenotes \fi
 \ifAJW@firstendnote
  \addtoendnotes{\protect\endnoteheading*{\AJW@endnotehead}}%
  %\addtoendnotes{\protect\noindent}%
 \fi
 \ifAJW@multisty\else
  \addtoendnotes{\protect\mark{\thepage}}%
 \fi
 \immediate\write\@enotes{\@doanenote{\@theenmark}}%
 \begingroup
  \def\next{#1}%
  \newlinechar='40
  \immediate\write\@enotes{\meaning\next}%
 \endgroup
 \immediate\write\@enotes{\@endanenote}%
 \edef\@currentlabel{\csname p@endnote\endcsname\@theenmark}%
}
\long\def\addtoendnotes#1{%
 \if@enotesopen \else \@openenotes \fi
 \begingroup
  \newlinechar='40
  \let\protect\string
  \immediate\write\@enotes{#1}%
 \endgroup
}
\def\endnotemark{\@ifnextchar [{\@xendnotemark
 }{\stepcounter{endnote}%
   \xdef\@theenmark{\theendnote}}%
 \@endnotemark
}
\def\@xendnotemark[#1]{\begingroup
  \c@endnote #1\relax
  \xdef\@theenmark{\theendnote}\endgroup
}
\def\@endnotemark{\leavevmode
 \ifhmode \edef\@x@sf{\the\spacefactor}\fi
 \@makeenmark
 \ifhmode\spacefactor\@x@sf\fi\relax
}
\def\endnotetext{\@ifnextchar [{\@xendnotenext
  }{\xdef\@theenmark{\theendnote}}%
 \@endnotetext
}
\def\@xendnotenext[#1]{\begingroup
  \c@endnote=#1\relax \xdef\@theenmark{\theendnote}\endgroup
}

\ifAJW@multisty
%\def\enoteheading{\section*{Notes}}
 \def\enoteheading{\endnoteheading*{Notes}}
\else
 \def\enoteheading{\chapter*{Notes}\leavevmode\par\vskip-\baselineskip}
\fi

\def\enoteformat{\raggedright\noindent
% \ifAJW@indentsty
%  \@tempdima = 3pc
% \else
  \@tempdima = 1em   %%EngD changed from 1.5em
% \fi
 \leftskip \@tempdima \parindent=0em%
 \leavevmode
 \hskip-\@tempdima
 \hskip-\parindent
 \hbox to \@tempdima {\@theenmark\hspace*{0.5em}}%   %%EngD changed
}
\ifAJW@multisty\else
 \def\AJW@enoteheadline{Notes to pp. \firstmark--\botmark}
\fi
\def\theendnotes{\immediate\closeout\@enotes
 \global\@enotesopenfalse
 \begingroup
  \makeatletter
  \def\@doanenote##1##2>{\def\@theenmark{##1}%\par removed
    \begingroup\enoteformat}%
  \def\@endanenote{\par\endgroup}%
  \def\ETC.{\@warning{[CUPBOOK] Some long endnotes will be truncated}
   \def\ETC.{\relax}}
  \enoteheading
  \ifAJW@multisty\else % redefine marking for running heads
   \def\chaptermark##1{}
   \def\leftmark{\AJW@enoteheadline}
   \def\rightmark{\AJW@enoteheadline}
  \fi
  \vspace*{-7pt}   %%EngD added
  \enotesize
  %\noindent
  \@input{\jobname.ent}%
  \ifAJW@multisty\else\clearpage\fi
 \endgroup
}

% Notes headings different from section heads
%\ifAJW@indentsty
% \def\endnoteheading{\@startsection {endnoteheading}{1}{\z@}
%   {10\p@ \@plus6\p@ \@minus3\p@}{6\p@}{\Aheadsize\bfseries\raggedright}}
%\else
% \def\endnoteheading{\@startsection {endnoteheading}{1}{\z@}
%   {10\p@ \@plus6\p@ \@minus3\p@}{6\p@}{\Aheadsize\bfseries\centering}}
%\fi

% endnote heads same as section heads
 \def\endnoteheading{\@startsection{endnoteheading}{2}{\z@}
  {-19.5\p@ \@plus-7\p@ \@minus-3.5\p@}{6.5\p@}{\Cheadsize\mediumseriffont\AJW@hangraggedright}}  %%EngD changed from Bhead



%    ****************************************
%    * FIGURES, TABLES, AND FLOATING BOXES  *
%    ****************************************
%
% Float placement parameters.
%
\setcounter{topnumber}{2}
\renewcommand\topfraction{0.9}
\setcounter{bottomnumber}{2}
\renewcommand\bottomfraction{0.5}
\setcounter{totalnumber}{3}
\renewcommand\textfraction{0.05}
\renewcommand\floatpagefraction{0.95}
\setcounter{dbltopnumber}{2}
\renewcommand\dbltopfraction{0.9}
\renewcommand\dblfloatpagefraction{0.8}
%
% \@makecaption{NUMBER}{TEXT} : Macro to make a figure or table caption.
%
\long\def\make@table@caption#1#2{\vspace{1.5\p@}%
  \setbox\@tempboxa\hbox%{{\rm #1\enskip\itshape #2}}%
    {\raggedright{\boldseriffont\indexsize #1}\enskip   %%EngD changed
    {\seriffont\tablesize #2\par}}
  \ifdim \wd\@tempboxa >\hsize
    {%\ifAJW@indentsty
        \raggedright
     %\else \centering\fi
     {\raggedright{\boldseriffont\indexsize #1}\enskip   %%EngD changed
    {\seriffont\tablesize #2\par}}
     }%
  \else
     %\ifAJW@indentsty
     %  \hbox to\hsize{\box\@tempboxa\hfill}\par
     %\else
       \hbox to\hsize{\hfil\box\@tempboxa\hfil}\par
     %\fi
  \fi
  \vspace*{2.5\p@}\par
}

\newbox\@figbox
\long\def\make@figure@caption#1#2{\vskip 10\p@
  \setbox\@tempboxa\hbox{\raggedright{\boldseriffont\indexsize #1}\enskip
    {\tablesize #2\par}}%   %%EngD changed
  \ifdim \wd\@tempboxa >\hsize
    {\raggedright{\boldseriffont\indexsize #1}\enskip {\tablesize #2\par}}%   %%EngD changed
  \else
%    \ifAJW@indentsty
       \hbox to\hsize{\box\@tempboxa\hfill}\par
%    \else
%      \hbox to\hsize{\hfil\box\@tempboxa\hfil}\par
%    \fi
  \fi
}

\long\def\make@floatingbox@caption#1#2{%\vspace{1.5\p@}%
  \setbox\@tempboxa\hbox{\raggedright
    {\boldseriffont\Cheadsize #1}\enskip    %%EngD changed from Bhead
    {\seriffont\Cheadsize #2\par}}   %%EngD changed from Bhead
  \ifdim \wd\@tempboxa >\hsize
    {%\ifAJW@indentsty
        \raggedright
     %\else \centering\fi
     {\raggedright
     {\boldseriffont\Cheadsize #1}\enskip   %%EngD changed from Bhead
    {\seriffont\Cheadsize #2\par}}   %%EngD changed from B head
     }%
  \else
     %\ifAJW@indentsty
     %  \hbox to\hsize{\box\@tempboxa\hfill}\par
     %\else
        \hbox to\hsize{\box\@tempboxa\hfil}\par
     %\fi
  \fi
  \vspace*{0.5\baselineskip}\par
}

%
% Float control
%
\newdimen\AJW@floatwidth

\def\AJW@float{\AJW@floatwidth \textwidth
 \addtolength{\AJW@floatwidth}{-\AJW@floatindent}
 \hspace*{\AJW@floatindent}\minipage{\AJW@floatwidth}%
 %\ifAJW@indentsty
 \raggedright
 %\else\centering\fi
 \smallish}
\def\endAJW@float{\endminipage}

% for floating boxes, we require \normalsize
\def\AJW@floatfb{\AJW@floatwidth \textwidth
 \addtolength{\AJW@floatwidth}{-\AJW@floatindent}
 \hspace*{\AJW@floatindent}\minipage{\AJW@floatwidth}%
 %\ifAJW@indentsty
 \raggedright
 %\else\centering\fi
 \normalsize}
\def\endAJW@floatfb{\endminipage}

\def\AJW@widefloat{\minipage{\textwidth}\raggedright\smallish}
\let\endAJW@widefloat=\endAJW@float
%
% FIGURE
%
\newcounter{figure}[chapter]
\def\thefigure{\thechapter.\@arabic\c@figure}
\def\fps@figure{tbp}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename\ \thefigure}

\def\figure{\let\@makecaption=\make@figure@caption
 \AJW@floatindent=\AJW@indent
 \@ifnextchar[{\AJW@figone}{\AJW@figzero}}
\def\AJW@figone[#1]{\@xfloat{figure}[#1]\AJW@float}
\edef\AJW@figzero{\noexpand\@xfloat{figure}[\fps@figure]\noexpand\AJW@float}
\def\endfigure{\endAJW@float\end@float}

\@namedef{figure*}{\let\@makecaption=\make@figure@caption
 \@ifnextchar[{\AJW@widefigone}{\AJW@widefigzero}}
\def\AJW@widefigone[#1]{\@xfloat{figure}[#1]\AJW@widefloat}
%\edef\AJW@widefigzero{\noexpand\@xfloat{figure}[p]\noexpand\AJW@widefloat}
\edef\AJW@widefigzero{\noexpand\@xfloat{figure}[\fps@figure]\noexpand
  \AJW@widefloat}
\@namedef{endfigure*}{\endAJW@widefloat\end@float}

\newcommand\widefigure{\hspace*{-17mm}}   %%EngD changed from -4pc

%
% TABLE
%
\newcounter{table}[chapter]
\def\thetable{\thechapter.\@arabic\c@table}
\def\fps@table{tbp}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename\ \thetable}

\newenvironment{table}{%
  \let\@makecaption=\make@table@caption
  \ifAJW@indentsty
    \ifdim\AJW@indent>\z@
      %\ifAJW@largesty
      %  \AJW@floatindent=\AJW@indent
      %\else
        \AJW@floatindent=\z@
      %\fi
    \fi
  \fi
  \@ifnextchar[{\AJW@tabone}{\AJW@tabzero}%
  }%
  {\endAJW@float\end@float}

\def\AJW@tabone[#1]{\@xfloat{table}[#1]\AJW@float}
\edef\AJW@tabzero{%
  \noexpand\@xfloat{table}[\fps@table]\noexpand\AJW@float}

\newenvironment{table*}{%
  \let\@makecaption=\make@table@caption
  \let\AJW@currenthline=\AJW@fullhline
  \@ifnextchar[{\AJW@widetabone}{\AJW@widetabzero}%
  }%
  {\endAJW@widefloat\end@float}

\def\AJW@widetabone[#1]{\@xfloat{table}[#1]\AJW@widefloat}
%\edef\AJW@widetabzero{\noexpand\@xfloat{table}[p]\noexpand\AJW@widefloat}
\edef\AJW@widetabzero{\noexpand\@xfloat{table}[\fps@table]\noexpand\AJW@widefloat}


%
% FLOATINGBOXES
%
% setting for frame
  \FrameRule=0.5pt
  \FrameSep=6pt

\newcounter{floatingbox}[chapter]
\def\thefloatingbox{\thechapter.\@arabic\c@floatingbox}
\def\fps@floatingbox{tbp}
\def\ftype@floatingbox{2}
\def\ext@floatingbox{lofb}
\def\fnum@floatingbox{\floatingboxname\ \thefloatingbox}

\newenvironment{floatingbox}{%
  \let\@makecaption=\make@floatingbox@caption
  \ifAJW@indentsty
    \ifdim\AJW@indent>\z@
      %\ifAJW@largesty
      %  \AJW@floatindent=\AJW@indent
      %\else
        \AJW@floatindent=\z@
      %\fi
    \fi
  \fi
  \@ifnextchar[{\AJW@fbone}{\AJW@fbzero}%
  }%
  {\vskip-2.1pt\end{framed}\endAJW@floatfb\end@float}  %%EngD changed

\def\AJW@fbone[#1]{\@xfloat{floatingbox}[#1]\AJW@floatfb\begin{framed}%
  \rightskip = 0pt%
  \leftskip  = 0pt%
  }

\edef\AJW@fbzero{%
  \noexpand\@xfloat{floatingbox}[\fps@floatingbox]\noexpand\AJW@floatfb\noexpand
    \begin{framed}%
      \rightskip = 0pt%
      \leftskip  = 0pt%
  }


%    ****************************************
%    *                TITLE                 *
%    ****************************************
%
% TITLEPAGE
%
\newif\ifAJW@titlepage
\def\titlepage{\@restonecolfalse
 \if@twocolumn\@restonecoltrue\onecolumn
  \else \newpage
 \fi
 \AJW@titlepagetrue
 \thispagestyle{empty}%
}
\def\endtitlepage{\if@restonecol\twocolumn \else \newpage \fi
  \AJW@titlepagefalse}
%
\def\maketitle{\openin1 \jobname.ttl
 \ifeof1
  \typeout{No file \jobname.ttl.}
  \begin{titlepage}
   %\ifAJW@indentsty
     \begin{indented}
   %\else \begin{center}
   %\fi
   %\ifAJW@largesty \vspace*{4\p@} \else
   \vspace*{2\p@}
   %\fi
   \parttitlesize\boldseriffont\@title \par
   \if\@subtitle\empty\else \vskip 14\p@ \subtitlesize\@subtitle \par \fi
   \vspace{34\p@}
   {\authorsize\seriffont
     \ifAJW@multisty {\itshape Edited by} \\ \fi
   % \ifAJW@indentsty \begin{author@tabular}[t]{@{}l@{}}
   %   \else \begin{author@tabular}[t]{c}
   % \fi \@author
   % \end{author@tabular}\par}
% four lines above re-written to avoid clash with array.sty
     \@author
     \par}
   \vfill
   %\ifAJW@indentsty
     \end{indented}
   %\else \end{center}
   %\fi
  \end{titlepage}
  \cleardoublepage
 \else
  \closein1 \relax\@@input \jobname.ttl
 \fi
 \setcounter{footnote}{0} % Reset definitions
 \let\thanks\relax
 \gdef\@thanks{}
 \gdef\@author{}
 \gdef\@shortauthor{}
 \gdef\@title{}
 \let\maketitle\relax
}
\gdef\@title{\mbox{}}
\def\title{\@ifnextchar[ {\@titletwo}{\@titleone}}
\def\@titletwo[#1]#2{\gdef\@title{#2}\gdef\@subtitle{#1}}
\def\@titleone#1{\gdef\@title{#1}\gdef\@subtitle{}}
\def\subtitle#1{\gdef\@subtitle{#1}}
\gdef\@subtitle{}
\def\halftitle#1{\gdef\@halftitle{#1}}
\gdef\@halftitle{\@title}
%
\gdef\@author{\mbox{}}
\gdef\@shortauthor{}
\def\author{
  \@ifnextchar[{\@authortwo}{\@authorone}%
}
\def\@authortwo[#1]#2{\gdef\@author{#2}\gdef\@shortauthor{#1}}
\def\@authorone#1{\gdef\@author{#1}\gdef\@shortauthor{}}

\def\author@tabular{\def\@halignto{}\@authortable}

\let\endauthor@tabular=\endtabular

\def\@authortable{\leavevmode \hbox \bgroup $\let\@acol=\@tabacol
 \let\@classz=\@tabclassz \let\@classiv=\@tabclassiv
 \ifAJW@titlepage \let\\=\AJWauthor@tabcrone
            \else \let\\=\author@tabcrone
 \fi
 \ignorespaces \@tabarray}
\def\author@tabcrone{{\ifnum0=`}\fi\@xtabularcr\indexsize\seriffont
 \ifAJW@titlepage \let\\=\AJWauthor@tabcrtwo
            \else \let\\=\author@tabcrtwo
 \fi
 \ignorespaces}
\def\author@tabcrtwo{{\ifnum0=`}\fi\@xtabularcr[-3\p@]\indexsize\seriffont
 \ifAJW@titlepage \let\\=\AJWauthor@tabcrtwo
            \else \let\\=\author@tabcrtwo
 \fi
 \ignorespaces}
\def\AJWauthor@tabcrone{{\ifnum0=`}\fi\@xtabularcr[-2\p@]\indexsize\seriffont
 \let\\=\AJWauthor@tabcrtwo\ignorespaces}
\def\AJWauthor@tabcrtwo{{\ifnum0=`}\fi\@xtabularcr[-5\p@]\indexsize\seriffont
 \let\\=\AJWauthor@tabcrtwo\ignorespaces}
\def\and{\end{author@tabular}\vskip 6\p@\par
 \begin{author@tabular}[t]{@{}l@{}}
   \begin{author@tabular}[t]{c}
 }

%    ****************************************
%    *            PAGE STYLES               *
%    ****************************************
%

\newcommand\copyrightline[1]{%
  \def\@copyrightline{#1}}

\edef\@copyrightline{\relax}

\newcommand\c@pyrightline[1]{%
  \gdef\@c@pyrightline{#1}}

\gdef\@c@pyrightline{%
  \vbox to 5.5\p@{\noindent
  \parbox[t]{\textwidth}{\normalfont\copyrightsize\baselineskip 9\p@
  \@copyrightline
  }%
  \vss}%
}


% Initializes TeX's marks
%
\mark{{}{}}
%
% centred \ps@plain and \ps@empty defined in LATEX.TEX
%
\ifAJW@indentsty
  \def\ps@plain{\let\@mkboth=\@gobbletwo
    \def\@oddfoot {\listsize\rm\hfill\thepage}
    \def\@evenfoot{\listsize\rm\thepage\hfill}
    \def\@oddhead {}
    \def\@evenhead{}}
\else
  \def\ps@plain{\leftskip\z@\let\@mkboth\@gobbletwo\vfuzz=5\p@
    \def\@oddhead{}%
    \def\@evenhead{}%
    \def\@oddfoot{\@c@pyrightline \hfil}%
    \def\@evenfoot{\@c@pyrightline \hfil}%
    \def\sectionmark##1{}%
    \def\subsectionmark##1{}%
 }
\fi


%
% Definition of 'headings' page style
%
\newlength\textwidthplus
\setlength\textwidthplus{\textwidth}
  \advance\textwidthplus\fourpica
%
\newcommand\@greyrunningheadrule{%
\setlength\fboxsep{1\p@}%
  \def\FrameCommand{\if@twocolumn\else\hspace*{-17mm}\fi\colorbox{thirtycolor}}%  %%EngD changed
  \parindent0pt%
% frame needs to be \hsize + 17mm - 2x \fboxsep   %%EngD changed
  \MakeFramed{%
    \if@twocolumn\hsize=\textwidth\advance\hsize 2\fboxsep\fi%
    \advance\hsize-\width
    \advance\hsize\fourpica
    \advance\hsize-2\fboxsep
    \@parboxrestore
    }%
  \endMakeFramed
}

\newcommand\@insertgreyrunningheadrule{%
  {\parindent0pt\par\vbox to .5pt{%
    \if@twocolumn\hskip5.4pc\fi%
    \hbox to\textwidthplus{%
      \vbox{\@greyrunningheadrule}%
    }\par
  }}%
}

\def\ps@headings{%
 \let\@mkboth=\markboth
 \def\@oddfoot {}
 \def\@evenfoot{}
  %
%%EngD changed definitions of oddhead and evenhead below
\def\@oddhead{%
   \hbox to \textwidth{\hfil\runningheadsize\boldseriffont{\leftmark}\hspace*{17mm}%
     \llap{\runningheadsize\lightseriffont\thepage}}%
   \hspace*{-\textwidth}{\@insertgreyrunningheadrule}%
   }%
 \def\@evenhead{\hspace*{-17mm}\hbox to 17mm{%
   \runningheadsize\lightseriffont\thepage\hfil}%
   \hbox to \textwidthplus{\runningheadsize%
     {\boldseriffont
       {\rightmark}\hfil}}%
     \hspace*{-\textwidthplus}{\@insertgreyrunningheadrule}%
    }%
  \ifAJW@multisty
  \def\chaptermark##1{}
  \def\sectionmark##1{}
 \else
  \def\chaptermark##1{\markboth{##1}{##1}}
  \def\sectionmark##1{\markright{\ifnum \c@secnumdepth >\z@
   \thesection\hspace{0.5em}\fi ##1}}%
 \fi
}

%
% Definition of 'myheadings' page style.
%
\def\ps@myheadings{\let\@mkboth=\@gobbletwo
 \def\@oddfoot {}
 \def\@evenfoot{}
%%EngD changed definitions of oddhead and evenhead below
 \def\@oddhead{%
   \hbox to \textwidth{\hfil\boldseriffont{\rightmark}\hspace*{17mm}%
     \llap{\runningheadsize\lightseriffont\thepage}}%
   \hspace*{-\textwidth}{\@insertgreyrunningheadrule}%
 }%
 \def\@evenhead{\hspace*{-17mm}\hbox to 17mm{%
   \runningheadsize\lightseriffont\thepage\hfil}%
   \hbox to \textwidthplus{%
     {\boldseriffont
       {\leftmark}\hfil}}%
     \hspace*{-\textwidthplus}{\@insertgreyrunningheadrule}%
 }%
 \def\chaptermark##1{}
 \def\sectionmark##1{}
 \def\subsectionmark##1{}}


\newcommand\contentsname{Contents}
\newcommand\listfigurename{Illustrations}
\newcommand\listtablename{Tables}
\newcommand\listfloatingboxes{Boxes}
\newcommand\listcontribname{List of contributors}
\newcommand\bibname{Bibliography}
\newcommand\refname{References}
\newcommand\indexname{Index}
\newcommand\figurename{Figure}
\newcommand\tablename{Table}
\newcommand\floatingboxname{Box}
\newcommand\partname{Part}
\newcommand\chaptername{Chapter}
\newcommand\appendixname{Appendix}

%    ****************************************
%    *            MISCELLANEOUS             *
%    ****************************************
%
% DATE
%
\def\today{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}
%
% EQUATION and EQNARRAY -- put here because it must follow \chapter definition
%
\@addtoreset{equation}{chapter}
\def\theequation{\thechapter.\arabic{equation}}
% \jot = 3\p@ % Extra space added between lines of an eqnarray environment
%
% Flush left / indented equations
%
% FLEQN DOCUMENT-STYLE OPTION - released 9 March 1987
%    for LaTeX version 2.09
% Copyright (C) 1987 by Leslie Lamport
%
\ifAJW@indentsty
 \def\[{\relax\ifmmode\@badmath\else\begin{trivlist}\item[]\leavevmode
  \hbox to\linewidth\bgroup$ \displaystyle \hskip\AJW@indent\bgroup\fi}
 \def\]{\relax\ifmmode \egroup $\hfil \egroup \end{trivlist}%
  \else \@badmath \fi}
 \def\equation{\refstepcounter{equation}\trivlist \item[]\leavevmode
  \hbox to\linewidth\bgroup $ \displaystyle \hskip\AJW@indent}
 \def\endequation{$\hfil \displaywidth\linewidth\@eqnnum\egroup \endtrivlist}
 \def\eqnarray{\stepcounter{equation}\let\@currentlabel=\theequation
  \global\@eqnswtrue
  \global\@eqcnt\z@\tabskip\AJW@indent\let\\=\@eqncr
  \abovedisplayskip\topsep\ifvmode\advance\abovedisplayskip\partopsep\fi
  \belowdisplayskip\abovedisplayskip
  \belowdisplayshortskip\abovedisplayskip
  \abovedisplayshortskip\abovedisplayskip
  $$\halign
   to \linewidth\bgroup\@eqnsel\hskip\@centering$\displaystyle\tabskip\z@
   {##}$&\global\@eqcnt\@ne \hskip 2\arraycolsep \hfil${##}$\hfil
   &\global\@eqcnt\tw@ \hskip 2\arraycolsep $\displaystyle{##}$\hfil
    \tabskip\@centering&\llap{##}\tabskip\z@\cr}
 \def\endeqnarray{\@@eqncr\egroup
  \global\advance\c@equation\m@ne$$\global\@ignoretrue}%
\else
 \renewenvironment{eqnarray} % remove exta space around alignment marks
  {\stepcounter{equation}%
   \let\@currentlabel=\theequation
   \global\@eqnswtrue
   \m@th
   \global\@eqcnt\z@
   \tabskip\@centering
   \let\\\@eqncr
   $$\everycr{}\halign to \displaywidth\bgroup
     \hskip\@centering$\displaystyle\tabskip\z@skip{##}$\@eqnsel
   &\global\@eqcnt\@ne \hfil${\;##\;}$\hfil
   &\global\@eqcnt\tw@
     $\displaystyle\tabskip\z@skip{##}$\hfil \tabskip\@centering
   &\hb@xt@\z@\bgroup\hss##\egroup
     \tabskip\z@skip \cr}
  {\@@eqncr\egroup\global\advance\c@equation\m@ne
   $$\global\@ignoretrue}
\fi

% this allows proof boxes to be right-aligned in eqnarrays
\newcommand\arrayqed{\arrayqedtrue}
\newcommand\arrayqedhere{\arrayqedfalse}
\newif\ifarrayqed

\def\@@eqncr{\let\reserved@a\relax
    \ifcase\@eqcnt \def\reserved@a{& & &}\or \def\reserved@a{& &}%
     \or \def\reserved@a{&}\else
       \let\reserved@a\@empty
       \@latex@error{Too many columns in eqnarray environment}\@ehc\fi
     \reserved@a
     \if@eqnsw
       \ifarrayqed
         \qedsymbol%
       \else
         \@eqnnum\stepcounter{equation}%
       \fi
    \fi
     \global\@eqnswtrue\global\@eqcnt\z@\cr}


%    ****************************************
%    *            CUP SPECIALS              *
%    ****************************************
%
% redefinition of \cleardoublepage to get blank empty pages
%
\def\clearemptypage{\clearpage
 \if@twoside
  \ifodd\c@page
   \else
    \thispagestyle{empty}
    \mbox{}\newpage
    \if@twocolumn
      \hbox{}\newpage
    \fi
  \fi
 \fi
}
\let\cleardoublepage=\clearemptypage
%
% redefinition of sections to move section numbers into a 17mm margin    %%EngD changed to 17mm from 4pc
%
\def\@sect#1#2#3#4#5#6[#7]#8{%
 \ifnum #2>\c@secnumdepth % LEVEL -- is heading to be numbered?
  \def\@svsec{}%
 \else
  \refstepcounter{#1}
  %\edef\@svsec{\csname the#1\endcsname\hskip 0.5em}
   \edef\@svsec{\hskip-17mm\hbox to0pt{\csname the#1\endcsname}\hskip 17mm}  %%EngD changed to 17mm from 4pc
 \fi
 \@tempskipa #5\relax % AFTERSKIP (-ve indicates a run-in heading)
 \ifdim \@tempskipa>\z@
  \begingroup #6\relax
   \@hangfrom{\hskip #3\relax\@svsec}{\interlinepenalty \@M #8\par}
  \endgroup
  \csname #1mark\endcsname{#7}%
  \def\leftmark{\thesection\hspace{0.5em}#7}
  \addcontentsline{toc}{#1}{\ifnum #2>\c@secnumdepth \else
   \protect\numberline{\csname the#1\endcsname}\fi #7}
 \else
  \def\@svsechd{#6\hskip #3\@svsec #8%
   \csname #1mark\endcsname{#7}%
   \def\leftmark{\thesection\hspace{0.5em}#7}
   \addcontentsline{toc}{#1}{\ifnum #2>\c@secnumdepth \else
    \protect\numberline{\csname the#1\endcsname}\fi#7}}%
 \fi
 \@xsect{#5}}


% remove the vertical rules in tables, by default
\def\AJW@fullhline{\noalign{\ifnum0=`}\fi \vskip 3\p@
  \hrule \@height\arrayrulewidth width\textwidth
  \vskip 3\p@
  \futurelet \@tempa\@xhline}
\def\AJW@hline{\noalign{\ifnum0=`}\fi
  \vskip 3\p@
  \hrule \@height \arrayrulewidth
  \vskip 3\p@
  \futurelet \@tempa\@xhline}
\let\AJW@currenthline=\AJW@hline
%
\def\tabular{\def\@halignto{}%
  \let\hline=\AJW@currenthline
  \def\@xhline{\ifx\@tempa\hline
    \vskip -6\p@
    \vskip \doublerulesep
  \fi
  \ifnum0=`{\fi}}%
  \def\@arrayrule{\@addtopreamble{\hskip -.5\arrayrulewidth
%   \vrule \@width \arrayrulewidth
    \hskip .5\arrayrulewidth}}%
  \@tabular}


% If authors really need to re-instate the rules,

\newcommand\reinstaterules{%
  \def\tabular{\let\@halignto\@empty\@tabular}%
}

% EXTRACT
%
\newenvironment{extract}{%
  \smallish
  \trivlist\leftskip \z@\rightskip\z@\item}
  {\endlist}

% ABSTRACT
%
%%EngD renewed definition of extract
\renewenvironment{extract}
               {\let\source\extsource%
                \list{}{\leftmargin=\z@\topsep8.5pt\partopsep0pt%
                \rightmargin\z@%
                \parsep\z@\labelwidth=0pt%\parsep=5\p@
                \smallish\raggedright}%
                   \item\relax}
               {\endlist\addvspace{-2\p@}}

\newenvironment{abstract}{\section*{Abstract}\trivlist\item[]%
 }{\endtrivlist\addvspace{13\p@}}

\def\extsource#1{\space(#1)}  %%EngD added

\def\quotefont{\fontsize{9}{11}\selectfont}  %%EngD added
%
\renewenvironment{quote}%  %%EngD renewed definition of extract
{\bgroup\let\source\quotesource\list{}{\leftmargin1pc\rightmargin1pc}\item\quotefont\raggedright}
{\endlist\egroup}
%
\def\quotesource#1{\par\quotefont\raggedleft{\it(#1)}}  %%EngD added


\def\episource#1{\par\quotefont\raggedleft{\it(#1)}}  %%EngD added

\newenvironment{epigraph}   %%EngD added
               {\let\source\episource%
                \list{}{\leftmargin=\z@\topsep13pt\partopsep0pt%
                \rightmargin\z@%
                \parsep\z@\labelwidth=0pt%\parsep=5\p@
                \normalsize\raggedright}%
                   \item\relax\itshape}
               {\endlist}

\def\cquotesource#1{\par\quotefont\raggedleft{\it(#1)}}  %%EngD added

\newenvironment{chapquote}  %%EngD added
               {\let\source\cquotesource%
              \list{}{%
              \leftmargin=\z@\topsep13pt\partopsep0pt%
                \rightmargin\z@%
                \parsep\z@\labelwidth=0pt%\parsep=5\p@
                \normalsize\raggedright\rightmargin3pc}%
                   \item\relax%
                   \fontsize{9}{11}\selectfont\itshape}
               {\endlist}


%    ****************************************
%    *           INITIALIZATION             *
%    ****************************************
%
% Default initializations
%
\ifAJW@landscape \pagestyle{empty}\else \pagestyle{headings}\fi
\pagenumbering{arabic}
\onecolumn
\flushbottom
\frenchspacing


%%EngD added all below
%%%%%%%%%%%%%%%%%% FM  %%%%%%%%%%%%%%%%%%
%
\RequirePackage{soul}  %%EngD. why add here?
\sodef\fmauthorspaced{}{.11em}{0.5em plus.1em minus.1em}{0.5em plus.1em minus.1em}
%
\newenvironment{halftitlepage}
{\parindent=1em\thispagestyle{empty}\null\vspace*{-14pt}}%\parindent\z@
{\par}
%
\renewcommand\halftitle[1]{{\boldseriffont\fontsize{11.5}{14}\selectfont \noindent#1\par\vspace*{13bp}}}
\newcommand\halfsubtitle[1]{\par\vskip-6pt{\seriffont\fontsize{11}{13}\selectfont\noindent #1\par\vskip2pt}}
\newcommand\halfedition[1]{\par\vskip17.1pt{\fontsize{11}{11}\selectfont\centering \lowercase{{#1}}\par}}
\newcommand\halftitleauthor[1]{\vspace*{13.5bp}%
{\boldseriffont\fontsize{10}{13}\selectfont \noindent#1}}
\newcommand\halftitleblurb[1]{\par\vskip11pt{\fontsize{10}{13}\selectfont\noindent%\leftskip2pc\rightskip2pc
#1\par\vskip-0.5pt}}
%
\newcommand\seriestitle[1]{\noindent{\fontsize{10.5}{13}\selectfont\bfseries #1\par}}
\newcommand\serieshead[1]{\par\vskip6pt\noindent{\bfseries #1\par}}
\newenvironment{seriespage}
{\clearpage\thispagestyle{empty}\null\vspace*{-12pt}\fontsize{9}{12}\selectfont}%\parindent\z@
{\par}
%
\newenvironment{serieslist}[1][\relax]
{\bgroup%
\def\itemize{\ifnum \@itemdepth >3 \@toodeep \else
  \advance\@itemdepth \@ne
  \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
%  \listsize
  \setlength\leftmargini{1em}%
  \list{}%
  {%\def\makelabel##1{{##1}}
   \topsep0\p@\labelsep 0pt\itemsep\z@\parsep\z@%
   \itemindent -9pt%
   \labelwidth 0pt%
   \leftmargin 9pt%
   }%
 \fi
}
\begin{itemize}}%\parindent\z@
{\end{itemize}\egroup}
%
%
\renewenvironment{titlepage}
{\cleardoublepage\null\vspace*{-20.75pt}\parindent\z@\thispagestyle{empty}}
{\par}
%
\newcommand\fmtitle[1]{{\boldseriffont\fontsize{26}{29}\raggedright\selectfont #1\vphantom{yp}\par}}
\newcommand\fmtitleaffil[1]{\par\vskip0.2pt{\fontsize{9}{13}\selectfont\itshape#1}\par\vskip-21.8pt}
\newcommand\fmsubtitle[1]{\par\vskip14.2pt{\seriffont\fontsize{18}{22}\selectfont #1\vphantom{yp}\par}}
\newcommand\fmedition[1]{\par\vskip22.1pt{\fontsize{12}{14}\selectfont\centering{\lowercase{\fmauthorspaced{#1}}}\vphantom{yp}\par}}
\newcommand\fmeditedby{\par\vskip35.1pt{\fontsize{11}{13}\selectfont\seriffont Edited by\vphantom{yp}\par\vskip-24bp}}%\centering
\newcommand\fmtitleauthor[1]{\par\vskip34pt{\seriffont\fontsize{12}{14}\selectfont\MakeUppercase{\fmauthorspaced{#1}}\vphantom{yp}\par}}
\def\cuplogo{\vfill\includegraphics{CamLogoTtl.eps}\par\vskip15.5pt}
%
\DeclareFontEncoding{TS1}{}{}
\AtBeginDocument{\def\textcopyright{{\reset@font\fontencoding{TS1}\fontfamily{\rmdefault}\selectfont\char"A9}}}
%
\newcommand\copyrightpagefont{\fontsize{8}{10}\selectfont}
%
\def\copyrightlogo{\includegraphics{CUP_Imprints_logo.eps}}
%
\newenvironment{copyrightpage}
{\clearpage\thispagestyle{empty}\parindent\z@\copyrightpagefont\raggedright}
{\par}
\def\onelinespace{\vspace*{\baselineskip}}
\def\halflinespace{\vspace*{.5\baselineskip}}
%
\newcommand\dedicationfont{\boldseriffont\fontsize{11}{15}\selectfont}
%
\newenvironment{dedication}
{\clearpage\thispagestyle{empty}\parindent\z@\dedicationfont\raggedright\null\vskip9.6pc}
{\par}

\newenvironment{contriblist}{\parindent\z@\raggedright}{\par}
\renewcommand\contributor[1]{{\boldseriffont\fontsize{10}{13}\selectfont#1\par\vskip\baselineskip}}
\renewcommand\affiliation[1]{\par{\normalfont{#1\par}}}
\long\def\contributors#1#2{{\boldseriffont\fontsize{10}{13}\selectfont#1\vphantom{Ay}\par}%
\removelastskip\nointerlineskip\vskip4.5pt\nointerlineskip%
{#2\vphantom{Ay}}\par\vskip\baselineskip}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Croppage pdf %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
\@ifpackageloaded{crop}{%
        \if@croppage\let\CROP@@@marks\vfil%
          \CROP@size{\paperwidth}{\paperheight}%
        \fi%
}{%
    \RequirePackage[dvips]{crop}
        \if@croppage%
            \CROP@size{\paperwidth}{\paperheight}%
        \fi%
}
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Trim and Media Box values in Croped pdf %%%%%%%%%%%%%%%%%%%
%
\newdimen\bkcropwidth
\newdimen\bkcropheight
\newdimen\bktrimwidth
\newdimen\bktrimheight
\newdimen\bkbleed

\gdef\booktrimwidth#1{\global\bktrimwidth=#1\global\bkcropwidth=#1\global\advance\bkcropwidth-.64mm\global\advance\bktrimwidth-.64mm}%
\gdef\booktrimheight#1{\global\bktrimheight=#1\global\bkcropheight=#1\global\advance\bkcropheight-.915mm\global\advance\bktrimheight-.915mm}%
\gdef\bookbleed#1{\global\bkbleed=#1\advance\bkcropwidth\bkbleed\advance\bkcropwidth\bkbleed\advance\bkcropheight\bkbleed\advance\bkcropheight\bkbleed}
%
%\booktrimwidth{\stockwidth}
%\booktrimheight{\stockheight}
\if@nobleed%
    \bookbleed{0mm}%
\else%
    \bookbleed{3mm}\AtBeginDocument{\addtolength{\oddsidemargin}{3mm}\addtolength{\evensidemargin}{3mm}}%
\fi%
%
%
\if@croppage
\newdimen\CP@toff@wd
\newdimen\CP@toff@ht
%
\newdimen\CP@boff@wd
\newdimen\CP@boff@ht
%
\newdimen\CP@crop@wd
\newdimen\CP@crop@ht
%
\newdimen\CP@bled@wd
\newdimen\CP@bled@ht
%
\newdimen\CP@trim@wd
\newdimen\CP@trim@ht
%
\def\str@yes{yes}
\def\SetCrop#1#2{%
    \gdef\IsCropSet{yes}
    \global\CP@crop@wd=#1\relax
    \global\CP@crop@ht=#2\relax}
\def\SetTrim#1#2{%
    \gdef\IsTrimSet{yes}
    \global\CP@trim@wd=#1\relax
    \global\CP@trim@ht=#2\relax}
\def\SetBleed#1#2{%
    \gdef\IsBleedSet{yes}
    \global\CP@bled@wd=#1\relax
    \global\CP@bled@ht=#2\relax}
%
\everyjob\expandafter{%
    \the\everyjob
    \typeout{%
        \filename\space <\filedate>^^J
        Version: v\fileversion^^J
        LaTeX macros for setting Page Box parameters
    }%
  \IfFileExists{\filename.cfg}{%
        \begingroup\@@input\@filef@und\endgroup
    }{%
        \typeout{No File: \filename.cfg}%
    }%
  \IfFileExists{\jobname.cfg}{%
        \begingroup\@@input\@filef@und\endgroup
    }{%
        \typeout{No File: \jobname.cfg}%
    }%
}
%
\AtBeginDocument{%
\SetCrop{\bkcropwidth}{\bkcropheight}%
\SetTrim{\bktrimwidth}{\bktrimheight}%
\SetBleed{\bkbleed}{\bkbleed}}
%
\def\do@pagebox@calc{%
    \CP@toff@wd=\CP@crop@wd
    \advance\CP@toff@wd     by -\CP@trim@wd
    \divide\CP@toff@wd  by \tw@
%
    \CP@toff@ht=\CP@crop@ht
    \advance\CP@toff@ht     by -\CP@trim@ht
    \divide\CP@toff@ht  by \tw@
%
    \advance\CP@trim@wd     by \CP@toff@wd
    \advance\CP@trim@ht     by \CP@toff@ht
%
    \CP@boff@wd=\CP@toff@wd
    \advance\CP@boff@wd     by -\CP@bled@wd
%
    \CP@boff@ht=\CP@toff@ht
    \advance\CP@boff@ht     by -\CP@bled@ht
%
    \advance\CP@bled@wd     by \CP@trim@wd
    \advance\CP@bled@ht     by \CP@trim@ht
}
%
\def\pdf@page@parameters{%
    \ifx\IsCropSet\str@yes
        [{ThisPage} << /CropBox [0 0 \strip@pt\CP@crop@wd\space \strip@pt\CP@crop@ht] >> /PUT pdfmark
        [{ThisPage} << /MediaBox[0 0 \strip@pt\CP@crop@wd\space \strip@pt\CP@crop@ht] >> /PUT pdfmark
    \fi
    \ifx\IsTrimSet\str@yes
        [{ThisPage} << /TrimBox [\strip@pt\CP@toff@wd\space \strip@pt\CP@toff@ht\space \strip@pt\CP@trim@wd\space \strip@pt\CP@trim@ht] >> /PUT pdfmark
    \fi
    \ifx\IsBleedSet\str@yes
        [{ThisPage} << /BleedBox[\strip@pt\CP@boff@wd\space \strip@pt\CP@boff@ht\space \strip@pt\CP@bled@wd\space \strip@pt\CP@bled@ht] >> /PUT pdfmark
    \fi
%       [{ThisPage} << /ArtBox [0 0 595 842] >> /PUT pdfmark
}
%
\def\shipout@PageObjects{%
    \special{ps: \pdf@page@parameters}%
}
%%
\AtBeginDocument{%
    \do@pagebox@calc
    \let\org@begindvi\@begindvi
    \def\@begindvi{%
        \shipout@PageObjects
        \org@begindvi
        \global\let\@begindvi\rest@dvi@pages
    }%
}
\let\rest@dvi@pages\shipout@PageObjects
\fi
%
\sloppypar

\def\unnumlist{\ifnum \@itemdepth >3 \@toodeep \else
  \advance\@itemdepth \@ne
  \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
%  \listsize
  \setlength\leftmargini{0em}%
  \list{}%
  {\def\makelabel##1{\hbox to 1pc{##1\hfill}}
   \labelsep 0pt%
   \itemindent -8.2mm%
   \labelwidth 0pt%
   \leftmargin 4mm\itemsep\z@\parsep\z@%
   }%
 \fi
}

\def\endunnumlist{\endlist}

\setlength\parindent{4mm}  %%EngD changed from 1em
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Croppage pdf end macro's %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\endinput
