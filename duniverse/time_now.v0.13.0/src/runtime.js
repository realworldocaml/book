///////// TIME_NOW

//Provides: time_now_nanoseconds_since_unix_epoch_or_zero
//Requires: caml_int64_mul, caml_int64_of_float, caml_int64_of_int32
var ms_to_nano = caml_int64_of_int32(1000*1000);
function time_now_nanoseconds_since_unix_epoch_or_zero(){
    var ms = Date.now();
    // multiple by two - int63 integers are shifted to the left
    var ms_i63 = caml_int64_of_float(ms*2);
    return caml_int64_mul(ms_i63,ms_to_nano);
}
