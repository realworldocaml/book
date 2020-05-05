#include <stdint.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#include "bench_micro_stubs.h"

value f_i0_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i0);
}

value f_i1_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i1);
}

value f_i2_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i2);
}

value f_i3_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i3);
}

value f_i4_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i4);
}

value f_i5_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i5);
}

value f_i6_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i6);
}

value f_i7_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i7);
}

value f_i8_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i8);
}

value f_i9_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i9);
}

value f_i10_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i10);
}

value f_i11_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i11);
}

value f_i12_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i12);
}

value f_i13_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i13);
}

value f_i14_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i14);
}

value f_i15_ptr(value _) {
  return caml_copy_nativeint((intptr_t)(void *)f_i15);
}

value f_i0_caml(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_int(f_i0()));
}

value f_i1_caml(value i0) {
  CAMLparam1(i0);
  int ii0 = Int_val(i0);
  CAMLreturn(Val_int(f_i1(ii0)));
}

value f_i2_caml(value i0, value i1) {
  CAMLparam2(i0,i1);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  CAMLreturn(Val_int(f_i2(ii0,ii1)));
}

value f_i3_caml(value i0, value i1, value i2) {
  CAMLparam3(i0,i1,i2);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  CAMLreturn(Val_int(f_i3(ii0,ii1,ii2)));
}

value f_i4_caml(value i0, value i1, value i2, value i3) {
  CAMLparam4(i0,i1,i2,i3);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  CAMLreturn(Val_int(f_i4(ii0,ii1,ii2,ii3)));
}

value f_i5_caml(value i0, value i1, value i2, value i3, value i4) {
  CAMLparam5(i0,i1,i2,i3,i4);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  CAMLreturn(Val_int(f_i5(ii0,ii1,ii2,ii3,ii4)));
}

value f_i6_caml(value i0, value i1, value i2, value i3, value i4, value i5) {
  CAMLparam5(i0,i1,i2,i3,i4);
  CAMLxparam1(i5);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  CAMLreturn(Val_int(f_i6(ii0,ii1,ii2,ii3,ii4,ii5)));
}

value f_i7_caml(value i0, value i1, value i2, value i3, value i4, value i5, value i6) {
  CAMLparam5(i0,i1,i2,i3,i4);
  CAMLxparam2(i5,i6);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  CAMLreturn(Val_int(f_i7(ii0,ii1,ii2,ii3,ii4,ii5,ii6)));
}

value f_i8_caml(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7) {
  CAMLparam5(i0,i1,i2,i3,i4);
  CAMLxparam3(i5,i6,i7);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  CAMLreturn(Val_int(f_i8(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7)));
}

value f_i9_caml(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8) {
  CAMLparam5(i0,i1,i2,i3,i4);
  CAMLxparam4(i5,i6,i7,i8);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  CAMLreturn(Val_int(f_i9(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8)));
}

value f_i10_caml(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8, value i9) {
  CAMLparam5(i0,i1,i2,i3,i4);
  CAMLxparam5(i5,i6,i7,i8,i9);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  int ii9 = Int_val(i9);
  CAMLreturn(Val_int(f_i10(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8,ii9)));
}

value f_i11_caml(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8, value i9, value i10) {
  CAMLparam5(i0,i1,i2,i3,i4);
  CAMLxparam5(i5,i6,i7,i8,i9);
  CAMLxparam1(i10);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  int ii9 = Int_val(i9);
  int ii10= Int_val(i10);
  CAMLreturn(Val_int(f_i11(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8,ii9,ii10)));
}

value f_i12_caml(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8, value i9, value i10, value i11) {
  CAMLparam5(i0,i1,i2,i3,i4);
  CAMLxparam5(i5,i6,i7,i8,i9);
  CAMLxparam2(i10,i11);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  int ii9 = Int_val(i9);
  int ii10= Int_val(i10);
  int ii11= Int_val(i11);
  CAMLreturn(Val_int(f_i12(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8,ii9,ii10,ii11)));
}

value f_i13_caml(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8, value i9, value i10, value i11, value i12) {
  CAMLparam5(i0,i1,i2,i3,i4);
  CAMLxparam5(i5,i6,i7,i8,i9);
  CAMLxparam3(i10,i11,i12);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  int ii9 = Int_val(i9);
  int ii10= Int_val(i10);
  int ii11= Int_val(i11);
  int ii12= Int_val(i12);
  CAMLreturn(Val_int(f_i13(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8,ii9,ii10,ii11,ii12)));
}

value f_i14_caml(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8, value i9, value i10, value i11, value i12, value i13) {
  CAMLparam5(i0,i1,i2,i3,i4);
  CAMLxparam5(i5,i6,i7,i8,i9);
  CAMLxparam4(i10,i11,i12,i13);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  int ii9 = Int_val(i9);
  int ii10= Int_val(i10);
  int ii11= Int_val(i11);
  int ii12= Int_val(i12);
  int ii13= Int_val(i13);
  CAMLreturn(Val_int(f_i14(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8,ii9,ii10,ii11,ii12,ii13)));
}

value f_i15_caml(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8, value i9, value i10, value i11, value i12, value i13, value i14) {
  CAMLparam5(i0,i1,i2,i3,i4);
  CAMLxparam5(i5,i6,i7,i8,i9);
  CAMLxparam5(i10,i11,i12,i13,i14);
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  int ii9 = Int_val(i9);
  int ii10= Int_val(i10);
  int ii11= Int_val(i11);
  int ii12= Int_val(i12);
  int ii13= Int_val(i13);
  int ii14= Int_val(i14);
  CAMLreturn(Val_int(f_i15(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8,ii9,ii10,ii11,ii12,ii13,ii14)));
}

value f_i0_cowboy(value unit) {
  return Val_int(f_i0());
}

value f_i1_cowboy(value i0) {
  int ii0 = Int_val(i0);
  return Val_int(f_i1(ii0));
}

value f_i2_cowboy(value i0, value i1) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  return Val_int(f_i2(ii0,ii1));
}

value f_i3_cowboy(value i0, value i1, value i2) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  return Val_int(f_i3(ii0,ii1,ii2));
}

value f_i4_cowboy(value i0, value i1, value i2, value i3) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  return Val_int(f_i4(ii0,ii1,ii2,ii3));
}

value f_i5_cowboy(value i0, value i1, value i2, value i3, value i4) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  return Val_int(f_i5(ii0,ii1,ii2,ii3,ii4));
}

value f_i6_cowboy(value i0, value i1, value i2, value i3, value i4, value i5) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  return Val_int(f_i6(ii0,ii1,ii2,ii3,ii4,ii5));
}

value f_i7_cowboy(value i0, value i1, value i2, value i3, value i4, value i5, value i6) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  return Val_int(f_i7(ii0,ii1,ii2,ii3,ii4,ii5,ii6));
}

value f_i8_cowboy(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  return Val_int(f_i8(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7));
}

value f_i9_cowboy(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  return Val_int(f_i9(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8));
}

value f_i10_cowboy(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8, value i9) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  int ii9 = Int_val(i9);
  return Val_int(f_i10(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8,ii9));
}

value f_i11_cowboy(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8, value i9, value i10) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  int ii9 = Int_val(i9);
  int ii10= Int_val(i10);
  return Val_int(f_i11(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8,ii9,ii10));
}

value f_i12_cowboy(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8, value i9, value i10, value i11) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  int ii9 = Int_val(i9);
  int ii10= Int_val(i10);
  int ii11= Int_val(i11);
  return Val_int(f_i12(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8,ii9,ii10,ii11));
}

value f_i13_cowboy(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8, value i9, value i10, value i11, value i12) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  int ii9 = Int_val(i9);
  int ii10= Int_val(i10);
  int ii11= Int_val(i11);
  int ii12= Int_val(i12);
  return Val_int(f_i13(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8,ii9,ii10,ii11,ii12));
}

value f_i14_cowboy(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8, value i9, value i10, value i11, value i12, value i13) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  int ii9 = Int_val(i9);
  int ii10= Int_val(i10);
  int ii11= Int_val(i11);
  int ii12= Int_val(i12);
  int ii13= Int_val(i13);
  return Val_int(f_i14(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8,ii9,ii10,ii11,ii12,ii13));
}

value f_i15_cowboy(value i0, value i1, value i2, value i3, value i4, value i5, value i6, value i7, value i8, value i9, value i10, value i11, value i12, value i13, value i14) {
  int ii0 = Int_val(i0);
  int ii1 = Int_val(i1);
  int ii2 = Int_val(i2);
  int ii3 = Int_val(i3);
  int ii4 = Int_val(i4);
  int ii5 = Int_val(i5);
  int ii6 = Int_val(i6);
  int ii7 = Int_val(i7);
  int ii8 = Int_val(i8);
  int ii9 = Int_val(i9);
  int ii10= Int_val(i10);
  int ii11= Int_val(i11);
  int ii12= Int_val(i12);
  int ii13= Int_val(i13);
  int ii14= Int_val(i14);
  return Val_int(f_i15(ii0,ii1,ii2,ii3,ii4,ii5,ii6,ii7,ii8,ii9,ii10,ii11,ii12,ii13,ii14));
}
