  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ dune exec ./test.exe --instrument-with bisect_ppx
  $ bisect-ppx-report html --verbose
  Info: found *.coverage files in './'
  Info: Writing index file...
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
      <meta charset="utf-8"/>
      <title>Coverage report</title>
      <meta name="description" content="33.33% coverage overall"/>
      <link rel="stylesheet" type="text/css" href="coverage.css"/>
    </head>
    <body>
      <div id="header">
        <h1>Coverage report</h1>
        <h2>33.33%</h2>
      </div>
      <div id="files">
        <div>
          <span class="meter">
            <span class="covered" style="width: 33%"></span>
          </span>
          <span class="percentage">33%</span>
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
      <meta charset="utf-8"/>
      <title>test.ml &mdash; Coverage report</title>
      <meta name="description" content="33.33% coverage in test.ml">
      <link rel="stylesheet" href="coverage.css"/>
      <script src="highlight.pack.js"></script>
      <script>hljs.initHighlightingOnLoad();</script>
    </head>
    <body>
      <div id="header">
        <h1>
          <a href="index.html">
            <span class="dirname"></span>test.ml
          </a>
        </h1>
        <h2>33.33%</h2>
      </div>
      <div id="navbar">
        <span class="unvisited" style="top:31.25%"></span>
        <span class="unvisited" style="bottom:18.75%"></span>
        <span class="unvisited" style="bottom:12.50%"></span>
        <span class="unvisited" style="bottom:0.00%"></span>
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
  <a id="L9"></a><span > </span>
  <a id="L10"></a><span > </span>
  <a id="L11"></a><span > </span>
  <a id="L12"></a><span > </span>
  <a id="L13"></a><span class="unvisited"> </span>
  <a id="L14"></a><span class="unvisited"> </span>
  <a id="L15"></a><span > </span>
  <a id="L16"></a><span class="unvisited"> </span>
  </pre>
        </div>
        <div id="text-layer">
          <pre id="line-numbers">
  <a href="#L1"> 1</a>
  <a href="#L2"> 2</a>
  <a href="#L3"> 3</a>
  <a href="#L4"> 4</a>
  <a href="#L5"> 5</a>
  <a href="#L6"> 6</a>
  <a href="#L7"> 7</a>
  <a href="#L8"> 8</a>
  <a href="#L9"> 9</a>
  <a href="#L10">10</a>
  <a href="#L11">11</a>
  <a href="#L12">12</a>
  <a href="#L13">13</a>
  <a href="#L14">14</a>
  <a href="#L15">15</a>
  <a href="#L16">16</a>
  </pre>
  <pre><code class="ocaml">let f () =
    <span data-count="1">(</span>)
  
  let g () =
    <span data-count="0">(</span>)
  
  let () =
    <span data-count="1">f</span> ()
  
  (* Reproduces a HTML display bug that existed in development between 2.6.3 and
     2.7.0, starting with 1b8d7ec5985aa12a85e797e3d53fc72713e80c35. *)
  let a () =
    <span data-count="0">i</span>f true then
      <span data-count="0">t</span>rue
    else
      <span data-count="0">f</span>alse
  </code></pre>
        </div>
      </div>
      <script src="coverage.js"></script>
    </body>
  </html>
