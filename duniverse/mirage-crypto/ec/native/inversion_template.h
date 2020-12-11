#define MAKE_FN_NAME1(x,y) x ## y
#define MAKE_FN_NAME(x,y) MAKE_FN_NAME1(x,y)

#define PRECOMP MAKE_FN_NAME(CURVE_DESCRIPTION,_divstep_precomp)
#define MSAT MAKE_FN_NAME(CURVE_DESCRIPTION,_msat)
#define MONE MAKE_FN_NAME(CURVE_DESCRIPTION,_set_one)
#define DIVSTEP MAKE_FN_NAME(CURVE_DESCRIPTION,_divstep)
#define OPP MAKE_FN_NAME(CURVE_DESCRIPTION,_opp)
#define MUL MAKE_FN_NAME(CURVE_DESCRIPTION,_mul)
#define SZNZ MAKE_FN_NAME(CURVE_DESCRIPTION,_selectznz)

#if LEN_PRIME < 46
#define ITERATIONS (((49 * LEN_PRIME) + 80) / 17)
#else
#define ITERATIONS (((49 * LEN_PRIME) + 57) / 17)
#endif

#define SAT_LIMBS LIMBS + 1 /* we might need 2 more bits to represent m in twos complement */
#define BYTES 8 * (((LEN_PRIME - 1) / 64) + 1)

static void inverse(WORD out[LIMBS], WORD g[SAT_LIMBS]) {

  WORD precomp[LIMBS];
  PRECOMP(precomp);

  WORD d = 1;
  WORD f[SAT_LIMBS];
  WORD v[LIMBS];
  WORD r[LIMBS];
  WORD out1;
  WORD out2[SAT_LIMBS], out3[SAT_LIMBS], out4[LIMBS], out5[LIMBS];

  MSAT(f);
  MONE(r);
  for (int j = 0; j < LIMBS; j++) v[j] = 0;

  for (int i = 0; i < ITERATIONS - (ITERATIONS % 2); i+=2) {
    DIVSTEP(&out1,out2,out3,out4,out5,d,f,g,v,r);
    DIVSTEP(&d,f,g,v,r,out1,out2,out3,out4,out5);
  }
  if (ITERATIONS % 2) {
    DIVSTEP(&out1,out2,out3,out4,out5,d,f,g,v,r);
    for (int k = 0; k < LIMBS; k++) v[k] = out4[k];
    for (int k = 0; k < SAT_LIMBS; k++) f[k] = out2[k];
  }

  WORD h[LIMBS];
  OPP(h, v);
  SZNZ(v, f[SAT_LIMBS -1 ] >> (WORDSIZE - 1), v, h);
  MUL(out, v, precomp);

  return;
}

static void inversion (WORD out[LIMBS], WORD in[LIMBS]) {
  WORD in_[SAT_LIMBS];
  for (int i = 0; i < LIMBS; i++) in_[i] = in[i];
  in_[LIMBS] = 0;
  inverse(out, in_);
  return;
}
