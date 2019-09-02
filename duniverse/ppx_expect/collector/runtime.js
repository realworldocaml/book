//Provides: expect_test_collector_saved_stdout
var expect_test_collector_saved_stdout 
//Provides: expect_test_collector_saved_stderr
var expect_test_collector_saved_stderr

//Provides: expect_test_collector_before_test
//Requires: caml_global_data, caml_ml_channels
//Requires: expect_test_collector_saved_stderr, expect_test_collector_saved_stdout
function expect_test_collector_before_test (voutput, vstdout, vstderr){
  expect_test_collector_saved_stderr = caml_ml_channels[vstderr];
  expect_test_collector_saved_stdout = caml_ml_channels[vstdout];
  var output = caml_ml_channels[voutput];
  caml_ml_channels[vstdout] = output;
  caml_ml_channels[vstderr] = output;
  return 0;
}

//Provides: expect_test_collector_after_test
//Requires: caml_global_data, caml_ml_channels
//Requires: expect_test_collector_saved_stderr, expect_test_collector_saved_stdout
function expect_test_collector_after_test (vstdout, vstderr){
  caml_ml_channels[vstdout] = expect_test_collector_saved_stdout;
  caml_ml_channels[vstderr] = expect_test_collector_saved_stderr;
  return 0;
}

//Provides:caml_out_channel_pos_fd
//Requires: caml_global_data, caml_ml_channels
function caml_out_channel_pos_fd(chan){
  var info = caml_ml_channels[chan];
  return info.offset
}
