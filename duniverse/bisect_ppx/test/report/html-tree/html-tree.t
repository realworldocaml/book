  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (include_subdirs unqualified)
  > 
  > (executable
  >   (name test_tree)
  >   (instrumentation (backend bisect_ppx)))
  > EOF
  $ dune exec ./test_tree.exe --instrument-with bisect_ppx
  $ bisect-ppx-report html --tree --verbose
  Info: found *.coverage files in './'
  Info: Writing index file...
  $ ls _coverage | sort
  baz
  coverage.css
  coverage.js
  foo
  highlight.pack.js
  index.html
  test_tree.ml.html
  $ ls _coverage/foo | sort
  bar
  foo.ml.html
  $ ls _coverage/foo/bar | sort
  bar_a.ml.html
  bar_b.ml.html
  $ ls _coverage/baz | sort
  baz.ml.html
  $ cat _coverage/index.html
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="utf-8"/>
      <title>Coverage report</title>
      <meta name="description" content="85.71% coverage overall"/>
      <link rel="stylesheet" type="text/css" href="coverage.css"/>
    </head>
    <body>
      <div id="header">
        <h1>Coverage report</h1>
        <h2>85.71%</h2>
      </div>
      <div id="files">
        <details open="">
          <summary>
          <span class="summary-indicator"></span>
          <div class="directory">
          <span class="meter">
            <span class="covered" style="width: 83%"></span>
          </span>
          <span class="percentage">83% <span class="stats">(5 / 6)</span></span>
          <span class="dirname">foo/</span>
          </div>
          </summary>
        <details open="">
          <summary>
          <span class="summary-indicator"></span>
          <div class="directory">
          <span class="meter">
            <span class="covered" style="width: 75%"></span>
          </span>
          <span class="percentage">75% <span class="stats">(3 / 4)</span></span>
          <span class="dirname">foo/bar/</span>
          </div>
          </summary>
        <div>
          <span class="summary-indicator"></span>
          <span class="meter">
            <span class="covered" style="width: 100%"></span>
          </span>
          <span class="percentage">100% <span class="stats">(2 / 2)</span></span>
          <a href="foo/bar/bar_a.ml.html">
            <span class="dirname">foo/bar/</span>bar_a.ml
          </a>
        </div>
        <div>
          <span class="summary-indicator"></span>
          <span class="meter">
            <span class="covered" style="width: 50%"></span>
          </span>
          <span class="percentage">50% <span class="stats">(1 / 2)</span></span>
          <a href="foo/bar/bar_b.ml.html">
            <span class="dirname">foo/bar/</span>bar_b.ml
          </a>
        </div>
        </details>
        <div>
          <span class="summary-indicator"></span>
          <span class="meter">
            <span class="covered" style="width: 100%"></span>
          </span>
          <span class="percentage">100% <span class="stats">(2 / 2)</span></span>
          <a href="foo/foo.ml.html">
            <span class="dirname">foo/</span>foo.ml
          </a>
        </div>
        </details>
        <details open="">
          <summary>
          <span class="summary-indicator"></span>
          <div class="directory">
          <span class="meter">
            <span class="covered" style="width: 50%"></span>
          </span>
          <span class="percentage">50% <span class="stats">(1 / 2)</span></span>
          <span class="dirname">baz/</span>
          </div>
          </summary>
        <div>
          <span class="summary-indicator"></span>
          <span class="meter">
            <span class="covered" style="width: 50%"></span>
          </span>
          <span class="percentage">50% <span class="stats">(1 / 2)</span></span>
          <a href="baz/baz.ml.html">
            <span class="dirname">baz/</span>baz.ml
          </a>
        </div>
        </details>
        <div>
          <span class="summary-indicator"></span>
          <span class="meter">
            <span class="covered" style="width: 100%"></span>
          </span>
          <span class="percentage">100% <span class="stats">(6 / 6)</span></span>
          <a href="test_tree.ml.html">
            <span class="dirname"></span>test_tree.ml
          </a>
        </div>
      </div>
      <script src="coverage.js"></script>
    </body>
  </html>
  $ cat _coverage/test_tree.ml.html
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="utf-8"/>
      <title>test_tree.ml &mdash; Coverage report</title>
      <meta name="description" content="100.00% coverage in test_tree.ml">
      <link rel="stylesheet" href="coverage.css"/>
      <script src="highlight.pack.js"></script>
      <script>hljs.initHighlightingOnLoad();</script>
    </head>
    <body>
      <div id="header">
        <h1>
          <a href="index.html">
            <span class="dirname"></span>test_tree.ml
          </a>
        </h1>
        <h2>100.00%</h2>
      </div>
      <div id="navbar">
      </div>
      <div id="report">
        <div id="lines-layer">
          <pre>
  <a id="L1"></a><span > </span>
  <a id="L2"></a><span class="visited"> </span>
  </pre>
        </div>
        <div id="text-layer">
          <pre id="line-numbers">
  <a href="#L1">1</a>
  <a href="#L2">2</a>
  </pre>
  <pre><code class="ocaml">let () =
    Foo.a () ; <span data-count="1">F</span>oo.b () ; <span data-count="1">B</span>ar_a.a () ; <span data-count="1">B</span>ar_a.b () ; <span data-count="1">B</span>ar_b.a () ; <span data-count="1">B</span>az.<span data-count="1">b</span> ()
  </code></pre>
        </div>
      </div>
      <script src="coverage.js"></script>
    </body>
  </html>
  $ cat _coverage/foo/foo.ml.html
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="utf-8"/>
      <title>foo.ml &mdash; Coverage report</title>
      <meta name="description" content="100.00% coverage in foo/foo.ml">
      <link rel="stylesheet" href="../coverage.css"/>
      <script src="../highlight.pack.js"></script>
      <script>hljs.initHighlightingOnLoad();</script>
    </head>
    <body>
      <div id="header">
        <h1>
          <a href="../index.html">
            <span class="dirname">foo/</span>foo.ml
          </a>
        </h1>
        <h2>100.00%</h2>
      </div>
      <div id="navbar">
      </div>
      <div id="report">
        <div id="lines-layer">
          <pre>
  <a id="L1"></a><span class="visited"> </span>
  <a id="L2"></a><span > </span>
  <a id="L3"></a><span class="visited"> </span>
  </pre>
        </div>
        <div id="text-layer">
          <pre id="line-numbers">
  <a href="#L1">1</a>
  <a href="#L2">2</a>
  <a href="#L3">3</a>
  </pre>
  <pre><code class="ocaml">let a () = <span data-count="1">(</span>)
  
  let b () = <span data-count="1">(</span>)
  </code></pre>
        </div>
      </div>
      <script src="../coverage.js"></script>
    </body>
  </html>
