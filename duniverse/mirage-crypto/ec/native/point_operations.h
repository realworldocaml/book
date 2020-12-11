#define MAKE_FN_NAME1(x,y) x ## y
#define MAKE_FN_NAME(x,y) MAKE_FN_NAME1(x,y)

#define fe_add MAKE_FN_NAME(CURVE_DESCRIPTION,_add)
#define fe_sub MAKE_FN_NAME(CURVE_DESCRIPTION,_sub)

#define fe_mul MAKE_FN_NAME(CURVE_DESCRIPTION,_mul)
#define fe_sqr MAKE_FN_NAME(CURVE_DESCRIPTION,_square)

#define fe_nonzero MAKE_FN_NAME(CURVE_DESCRIPTION,_nonzero)
#define fe_selectznz MAKE_FN_NAME(CURVE_DESCRIPTION,_selectznz)

typedef WORD fe[LIMBS];

static WORD fe_nz(const WORD in1[LIMBS]) {
  WORD ret;
  fe_nonzero(&ret, in1);
  return ret;
}

static void fe_copy(WORD out[LIMBS], const WORD in1[LIMBS]) {
  for (int i = 0; i < LIMBS; i++) {
    out[i] = in1[i];
  }
}

static void fe_cmovznz(WORD out[LIMBS], WORD t, const WORD z[LIMBS],
                       const WORD nz[LIMBS]) {
  fe_selectznz(out, !!t, z, nz);
}

// Group operations
// ----------------
//
// Building on top of the field operations we have the operations on the
// elliptic curve group itself. Points on the curve are represented in Jacobian
// coordinates.
//
// Both operations were transcribed to Coq and proven to correspond to naive
// implementations using Affine coordinates, for all suitable fields.  In the
// Coq proofs, issues of constant-time execution and memory layout (aliasing)
// conventions were not considered. Specification of affine coordinates:
// <https://github.com/mit-plv/fiat-crypto/blob/79f8b5f39ed609339f0233098dee1a3c4e6b3080/src/Spec/WeierstrassCurve.v#L28>
// As a sanity check, a proof that these points form a commutative group:
// <https://github.com/mit-plv/fiat-crypto/blob/79f8b5f39ed609339f0233098dee1a3c4e6b3080/src/Curves/Weierstrass/AffineProofs.v#L33>

// point_double calculates 2*(x_in, y_in, z_in)
//
// The method is taken from:
//   http://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-3.html#doubling-dbl-2001-b
//
// Coq transcription and correctness proof:
// <https://github.com/mit-plv/fiat-crypto/blob/79f8b5f39ed609339f0233098dee1a3c4e6b3080/src/Curves/Weierstrass/Jacobian.v#L93>
// <https://github.com/mit-plv/fiat-crypto/blob/79f8b5f39ed609339f0233098dee1a3c4e6b3080/src/Curves/Weierstrass/Jacobian.v#L201>
//
// Outputs can equal corresponding inputs, i.e., x_out == x_in is allowed.
// while x_out == y_in is not (maybe this works, but it's not tested).
static void point_double(fe x_out, fe y_out, fe z_out,
                         const fe x_in, const fe y_in, const fe z_in) {
  fe delta, gamma, beta, ftmp, ftmp2, tmptmp, alpha, fourbeta;
  // delta = z^2
  fe_sqr(delta, z_in);
  // gamma = y^2
  fe_sqr(gamma, y_in);
  // beta = x*gamma
  fe_mul(beta, x_in, gamma);

  // alpha = 3*(x-delta)*(x+delta)
  fe_sub(ftmp, x_in, delta);
  fe_add(ftmp2, x_in, delta);

  fe_add(tmptmp, ftmp2, ftmp2);
  fe_add(ftmp2, ftmp2, tmptmp);
  fe_mul(alpha, ftmp, ftmp2);

  // x' = alpha^2 - 8*beta
  fe_sqr(x_out, alpha);
  fe_add(fourbeta, beta, beta);
  fe_add(fourbeta, fourbeta, fourbeta);
  fe_add(tmptmp, fourbeta, fourbeta);
  fe_sub(x_out, x_out, tmptmp);

  // z' = (y + z)^2 - gamma - delta
  fe_add(delta, gamma, delta);
  fe_add(ftmp, y_in, z_in);
  fe_sqr(z_out, ftmp);
  fe_sub(z_out, z_out, delta);

  // y' = alpha*(4*beta - x') - 8*gamma^2
  fe_sub(y_out, fourbeta, x_out);
  fe_add(gamma, gamma, gamma);
  fe_sqr(gamma, gamma);
  fe_mul(y_out, alpha, y_out);
  fe_add(gamma, gamma, gamma);
  fe_sub(y_out, y_out, gamma);
}

// point_add calculates (x1, y1, z1) + (x2, y2, z2)
//
// The method is taken from:
//   http://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-3.html#addition-add-2007-bl,
// adapted for mixed addition (z2 = 1, or z2 = 0 for the point at infinity).
//
// Coq transcription and correctness proof:
// <https://github.com/mit-plv/fiat-crypto/blob/79f8b5f39ed609339f0233098dee1a3c4e6b3080/src/Curves/Weierstrass/Jacobian.v#L135>
// <https://github.com/mit-plv/fiat-crypto/blob/79f8b5f39ed609339f0233098dee1a3c4e6b3080/src/Curves/Weierstrass/Jacobian.v#L205>
//
// This function includes a branch for checking whether the two input points
// are equal, (while not equal to the point at infinity). This case never
// happens during single point multiplication, so there is no timing leak for
// ECDH or ECDSA signing.
static void point_add(fe x3, fe y3, fe z3, const fe x1,
                      const fe y1, const fe z1, const int mixed,
                      const fe x2, const fe y2, const fe z2) {
  fe x_out, y_out, z_out;
  WORD z1nz = fe_nz(z1);
  WORD z2nz = fe_nz(z2);

  // z1z1 = z1z1 = z1**2
  fe z1z1; fe_sqr(z1z1, z1);

  fe u1, s1, two_z1z2;
  if (!mixed) {
    // z2z2 = z2**2
    fe z2z2; fe_sqr(z2z2, z2);

    // u1 = x1*z2z2
    fe_mul(u1, x1, z2z2);

    // two_z1z2 = (z1 + z2)**2 - (z1z1 + z2z2) = 2z1z2
    fe_add(two_z1z2, z1, z2);
    fe_sqr(two_z1z2, two_z1z2);
    fe_sub(two_z1z2, two_z1z2, z1z1);
    fe_sub(two_z1z2, two_z1z2, z2z2);

    // s1 = y1 * z2**3
    fe_mul(s1, z2, z2z2);
    fe_mul(s1, s1, y1);
  } else {
    // We'll assume z2 = 1 (special case z2 = 0 is handled later).

    // u1 = x1*z2z2
    fe_copy(u1, x1);
    // two_z1z2 = 2z1z2
    fe_add(two_z1z2, z1, z1);
    // s1 = y1 * z2**3
    fe_copy(s1, y1);
  }

  // u2 = x2*z1z1
  fe u2; fe_mul(u2, x2, z1z1);

  // h = u2 - u1
  fe h; fe_sub(h, u2, u1);

  WORD xneq = fe_nz(h);

  // z_out = two_z1z2 * h
  fe_mul(z_out, h, two_z1z2);

  // z1z1z1 = z1 * z1z1
  fe z1z1z1; fe_mul(z1z1z1, z1, z1z1);

  // s2 = y2 * z1**3
  fe s2; fe_mul(s2, y2, z1z1z1);

  // r = (s2 - s1)*2
  fe r;
  fe_sub(r, s2, s1);
  fe_add(r, r, r);

  WORD yneq = fe_nz(r);

  if (!xneq && !yneq && z1nz && z2nz) {
    point_double(x3, y3, z3, x1, y1, z1);
    return;
  }

  // I = (2h)**2
  fe i;
  fe_add(i, h, h);
  fe_sqr(i, i);

  // J = h * I
  fe j; fe_mul(j, h, i);

  // V = U1 * I
  fe v; fe_mul(v, u1, i);

  // x_out = r**2 - J - 2V
  fe_sqr(x_out, r);
  fe_sub(x_out, x_out, j);
  fe_sub(x_out, x_out, v);
  fe_sub(x_out, x_out, v);

  // y_out = r(V-x_out) - 2 * s1 * J
  fe_sub(y_out, v, x_out);
  fe_mul(y_out, y_out, r);
  fe s1j;
  fe_mul(s1j, s1, j);
  fe_sub(y_out, y_out, s1j);
  fe_sub(y_out, y_out, s1j);

  fe_cmovznz(x_out, z1nz, x2, x_out);
  fe_cmovznz(x3, z2nz, x1, x_out);
  fe_cmovznz(y_out, z1nz, y2, y_out);
  fe_cmovznz(y3, z2nz, y1, y_out);
  fe_cmovznz(z_out, z1nz, z2, z_out);
  fe_cmovznz(z3, z2nz, z1, z_out);
}
