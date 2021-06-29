  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ dune exec ./test.exe --instrument-with bisect_ppx
  $ bisect-ppx-report html
  Info: found coverage files in './'
  $ ls _coverage | sort
  coverage.css
  coverage.js
  highlight.pack.js
  index.html
  test.ml.html
  $ cat _coverage/index.html
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <title>Coverage report</title>
      <link rel="stylesheet" type="text/css" href="coverage.css" />
      <meta charset="utf-8" />
    </head>
    <body>
      <div id="header">
        <h1>Coverage report</h1>
        <h2>66.66%</h2>
      </div>
      <div id="files">
        <div>
          <span class="meter">
            <span class="covered" style="width: 66%"></span>
          </span>
          <span class="percentage">66%</span>
          <a href="test.ml.html">
            <span class="dirname"></span>test.ml
          </a>
        </div>
      </div>
    </body>
  </html>
  $ cat _coverage/test.ml.html
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <title>Coverage report</title>
      <link rel="stylesheet" href="coverage.css" />
      <script src="highlight.pack.js"></script>
      <script>hljs.initHighlightingOnLoad();</script>
      <meta charset="utf-8" />
    </head>
    <body>
      <div id="header">
        <h1>
          <a href="index.html">
            <span class="dirname"></span>test.ml
          </a>
        </h1>
        <h2>66.67%</h2>
      </div>
      <div id="navbar">
        <span class="unvisited" style="bottom:37.50%"></span>
      </div>
      <div id="report">
        <div id="lines-layer">
          <pre>
  <a id="L1"></a><span > </span>
  <a id="L2"></a><span class="visited"> </span>
  <a id="L3"></a><span > </span>
  <a id="L4"></a><span > </span>
  <a id="L5"></a><span class="unvisited"> </span>
  <a id="L6"></a><span > </span>
  <a id="L7"></a><span > </span>
  <a id="L8"></a><span class="visited"> </span>
  </pre>
        </div>
        <div id="text-layer">
          <pre id="line-numbers">
  <a href="#L1">1</a>
  <a href="#L2">2</a>
  <a href="#L3">3</a>
  <a href="#L4">4</a>
  <a href="#L5">5</a>
  <a href="#L6">6</a>
  <a href="#L7">7</a>
  <a href="#L8">8</a>
  </pre>
  <pre><code class="ocaml">let f () =
    <span data-count="1">(</span>)
  
  let g () =
    <span data-count="0">(</span>)
  
  let () =
    <span data-count="1">f</span> ()
  </code></pre>
        </div>
      </div>
      <script src="coverage.js"></script>
    </body>
  </html>
