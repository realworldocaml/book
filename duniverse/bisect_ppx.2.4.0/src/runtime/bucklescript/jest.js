var runtime = require("bisect_ppx/lib/js/src/runtime/bucklescript/runtime.js");
afterAll(function () {
  runtime.write_coverage_data();
  runtime.reset_coverage_data();
});
