/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <inttypes.h>
#include <float.h>
#include <assert.h>
#include <string.h>
#include <errno.h>
#include <math.h>

#if defined _WIN32 && !defined __CYGWIN__
#include <windows.h>
#else
#include <pthread.h>
#include <unistd.h>
#if defined(__APPLE__)
#include <dispatch/dispatch.h>
#else
#include <semaphore.h>
#endif
#endif

#include "ctypes_complex_compatibility.h"
#include "test_functions.h"

static int add(int x, int y) { return x + y; }
static int times(int x, int y) { return x * y; }

int higher_order_1(intfun *callback, int x, int y)
{
  return callback(x, y) == x + y;
}

int higher_order_3(acceptor *callback, intfun *fn, int x, int y)
{
  return callback(fn, x, y);
}

int higher_order_simplest(vintfun *callback)
{
  return callback(22);
}

intfun *returning_funptr(int v)
{
  switch (v)
  {
  case 0: return add;
  case 1: return times;
  default: return NULL;
  }
}

int accepting_possibly_null_funptr(intfun *f, int x, int y)
{
  return f != NULL ? f(x, y) : -1;
}

int global = 100;

int *return_global_address(void)
{
  return &global;
}

double float_pointer_callback(void (*f)(double *), double v)
{
  f(&v);
  return v * 2.0;
}

int write_through_callback(int (*f)(int *))
{
  int x = 42;
  return f(&x) + x;
}

int write_through_callback_pointer_pointer(int (*f)(int **, int *))
{
  int x = 10, y = 20;
  int *p =&x;
  return f(&p, &y) + *p + x + y;
}

int is_null(void *p)
{
  return p == NULL;
}

int callback_returns_funptr(vintfun *(*callback)(int), int x)
{
  vintfun *v1 = callback(x);
  vintfun *v2 = callback(x + 1);

  return v1(10) + v2(20);
}


int *pass_pointer_through(int *a, int *b, int i)
{
  return (i >= 0) ? a : b;
}

int accept_struct(struct simple simple)
{
  return simple.i + (int)simple.f + (simple.self == NULL ? 1 : 0);
}

struct simple return_struct(void)
{
  struct simple *t = malloc(sizeof *t);
  t->i = 10;
  t->f = 12.5;
  t->self = t;

  struct simple s = {
    20,
    35.0,
    t
  };

  return s;
}

int64_t sum_union_components(union padded *padded, size_t len)
{
  size_t i;
  int64_t acc = 0;
  for (i = 0; i < len; i++) {
    acc += padded[i].i;
  }
  return acc;
}

union padded add_unions(union padded l, union padded r)
{
  union padded result, args[] = { l, r };
  result.i = sum_union_components(args, sizeof args / sizeof *args);
  return result;
}


void concat_strings(const char **sv, int sc, char *buffer)
{
  int i = 0;
  for (; i < sc; i++) {
    const char *s = sv[i];
    while (*s) {
      *buffer++ = *s++;
    }
  }
  *buffer = '\0';
}


struct tagged add_tagged_numbers(struct tagged l, struct tagged r)
{
  union number n;
  struct tagged result = { 'd', n };
  switch (l.tag) {
  case 'i':
    switch (r.tag) {
    case 'i':
      result.num.d = l.num.i + r.num.i;
      return result;
    case 'd':
      result.num.d = l.num.i + r.num.d;
      return result;
    default: assert(0);
    }
  case 'd':
    switch (r.tag) {
    case 'i':
      result.num.d = l.num.d + r.num.i;
      return result;
    case 'd':
      result.num.d = l.num.d + r.num.d;
      return result;
    default: assert(0);
    }
  default: assert(0);
  }
}


double accepts_pointer_to_array_of_structs(struct tagged(*arr)[5])
{
  double sum = 0.0;
  int i = 0;
  struct tagged *s = &(*arr[0]);
  for (; i < 5; i++) {
    switch (s[i].tag) {
    case 'i': {
      sum += s[i].num.i;
      break;
    }
    case 'd': {
      sum += s[i].num.d;
      break;
    }
    default: assert(0);
    }
  }
  return sum;
}

struct global_struct global_struct = { sizeof GLOBAL_STRING - 1, GLOBAL_STRING };


struct triple add_triples(struct triple l, struct triple r)
{
  int i = 0;
  struct triple result;
  for (; i < 3; i++) {
    result.elements[i] = l.elements[i] + r.elements[i];
  }
  return result;
}


/* OO-style example */
struct animal_methods;
struct animal {
  struct animal_methods *vtable;
};
struct animal_methods {
  char *(*say)(struct animal *);
  char *(*identify)(struct animal *);
};

int check_name(struct animal *a, char *name)
{
  return strcmp(a->vtable->identify(a), name) == 0;
}

enum colour { white, red, black, pale };

struct chorse_methods;

struct chorse {
  struct chorse_methods *vtable;
  enum colour colour;
};

struct chorse_methods {
  struct animal_methods base;
  char *(*colour)(struct chorse *);
};

char *chorse_colour(struct chorse *chorse)
{
  switch (chorse->colour) {
  case white : return "white";
  case red   : return "red";
  case black : return "black";
  case pale  : return "pale";
  default: assert(0);
  }
}

char *chorse_say(struct animal *c)
{
  return "neigh";
}

char *chorse_identify(struct animal *a)
{
  static char buffer[30]; /* static allocation is adequate for the test */
  sprintf(buffer, "%s horse", chorse_colour((struct chorse *)a));
  return buffer;
}

static struct chorse_methods chorse_vtable = {
  {
    chorse_say,
    chorse_identify,
  },
  chorse_colour,
};

struct chorse *new_chorse(int colour)
{
  struct chorse *h = malloc(sizeof *h);
  h->vtable = &chorse_vtable;
  h->colour = (enum colour)colour;
  return h;
}
/* (End of OO-style example) */

int accept_pointers(float *float_p,
                    double *double_p,
                    short *short_p,
                    int *int_p,
                    long *long_p,
                    long long *llong_p,
                    intnat *nativeint_p,
                    int8_t *int8_t_p,
                    int16_t *int16_t_p,
                    int32_t *int32_t_p,
                    int64_t *int64_t_p,
                    uint8_t *uint8_t_p,
                    uint16_t *uint16_t_p,
                    uint32_t *uint32_t_p,
                    uint64_t *uint64_t_p,
                    size_t *size_t_p,
                    unsigned short *ushort_p,
                    unsigned *uint_p,
                    unsigned long *ulong_p,
                    unsigned long long *ullong_p)
{
  return (*float_p
          + *double_p
          + *short_p
          + *int_p
          + *long_p
          + *llong_p
          + *nativeint_p
          + *int8_t_p
          + *int16_t_p
          + *int32_t_p
          + *int64_t_p
          + *uint8_t_p
          + *uint16_t_p
          + *uint32_t_p
          + *uint64_t_p
          + *size_t_p
          + *ushort_p
          + *uint_p
          + *ulong_p
          + *ullong_p);
}

int accept_pointers_to_pointers(int *p, int **pp, int ***ppp, int ****pppp)
{
  return *p + **pp + ***ppp + ****pppp;
}

intfun **returning_pointer_to_function_pointer(void)
{
  static intfun *f = times;
  return &f;
}

int accepting_pointer_to_function_pointer(intfun **pfp)
{
  return (*pfp)(20, 4);
}

int passing_pointers_to_callback(pintfun1 *f)
{
  int x = 3, y = 4;
  return f(&x, &y);
}

int accepting_pointer_from_callback(pintfun2 *f)
{
  int *p = f(7, 8);
  int q = *p;
  *p = 12;
  return q;
}


signed char retrieve_SCHAR_MIN(void) { return SCHAR_MIN; }
signed char retrieve_SCHAR_MAX(void) { return SCHAR_MAX; }
unsigned char retrieve_UCHAR_MAX(void) { return UCHAR_MAX; }
char retrieve_CHAR_MIN(void) { return CHAR_MIN; }
char retrieve_CHAR_MAX(void) { return CHAR_MAX; }
short retrieve_SHRT_MIN(void) { return SHRT_MIN; }
short retrieve_SHRT_MAX(void) { return SHRT_MAX; }
unsigned short retrieve_USHRT_MAX(void) { return USHRT_MAX; }
int retrieve_INT_MIN(void) { return INT_MIN; }
int retrieve_INT_MAX(void) { return INT_MAX; }
unsigned int retrieve_UINT_MAX(void) { return UINT_MAX; }
long retrieve_LONG_MAX(void) { return LONG_MAX; }
long retrieve_LONG_MIN(void) { return LONG_MIN; }
unsigned long retrieve_ULONG_MAX(void) { return ULONG_MAX; }
long long retrieve_LLONG_MAX(void) { return LLONG_MAX; }
long long retrieve_LLONG_MIN(void) { return LLONG_MIN; }
unsigned long long retrieve_ULLONG_MAX(void) { return ULLONG_MAX; }
int8_t retrieve_INT8_MIN(void) { return INT8_MIN; }
int16_t retrieve_INT16_MIN(void) { return INT16_MIN; }
int32_t retrieve_INT32_MIN(void) { return INT32_MIN; }
int64_t retrieve_INT64_MIN(void) { return INT64_MIN; }
int8_t retrieve_INT8_MAX(void) { return INT8_MAX; }
int16_t retrieve_INT16_MAX(void) { return INT16_MAX; }
int32_t retrieve_INT32_MAX(void) { return INT32_MAX; }
int64_t retrieve_INT64_MAX(void) { return INT64_MAX; }
uint8_t retrieve_UINT8_MAX(void) { return UINT8_MAX; }
uint16_t retrieve_UINT16_MAX(void) { return UINT16_MAX; }
uint32_t retrieve_UINT32_MAX(void) { return UINT32_MAX; }
uint64_t retrieve_UINT64_MAX(void) { return UINT64_MAX; }
size_t retrieve_SIZE_MAX(void) { return SIZE_MAX; }
float retrieve_FLT_MIN(void) { return FLT_MIN; }
float retrieve_FLT_MAX(void) { return FLT_MAX; }
double retrieve_DBL_MIN(void) { return DBL_MIN; }
double retrieve_DBL_MAX(void) { return DBL_MAX; }

void add_complexd(double _Complex *l, double _Complex *r, double _Complex *out)
{
  *out = *l + *r;
}

void mul_complexd(double _Complex *l, double _Complex *r, double _Complex *out)
{
  *out = *l * *r;
}

void rotdist_complexd(double _Complex *c, double *r, double *out) {
  double _Complex x = *c * (ctypes_compat_make_complex(cos(*r), sin(*r)));
  *out = fabs(ctypes_compat_creal(x)) + fabs(ctypes_compat_cimag(x));
}

void add_complexld(long double _Complex *l, long double _Complex *r, long double _Complex *out)
{
  *out = *l + *r;
}

void mul_complexld(long double _Complex *l, long double _Complex *r, long double _Complex *out)
{
  *out = *l * *r;
}

void rotdist_complexld(long double _Complex *c, long double *r, long double *out) {
  long double _Complex x = *c * (ctypes_compat_make_complexl(cosl(*r), sinl(*r)));
  *out = fabsl(ctypes_compat_creall(x)) + fabsl(ctypes_compat_cimagl(x));
}

void add_complexf(float _Complex *l, float _Complex *r, float _Complex *out)
{
  *out = *l + *r;
}

void mul_complexf(float _Complex *l, float _Complex *r, float _Complex *out)
{
  *out = *l * *r;
}

void rotdist_complexf(float _Complex *c, float *r, float *out) {
  float _Complex x = *c * (ctypes_compat_make_complexf(cosf(*r), sinf(*r)));
  *out = fabsf(ctypes_compat_crealf(x)) + fabsf(ctypes_compat_cimagf(x));
}

long double _Complex add_complexld_val(long double _Complex l, long double _Complex r)
{
  return l + r;
}

long double _Complex mul_complexld_val(long double _Complex l, long double _Complex r)
{
  return l * r;
}

long double rotdist_complexld_val(long double _Complex c, long double r) {
  long double _Complex x = c * (ctypes_compat_make_complexl(cosl(r), sinl(r)));
  return fabsl(ctypes_compat_creall(x)) + fabsl(ctypes_compat_cimagl(x));
}

double _Complex add_complexd_val(double _Complex l, double _Complex r)
{
  return l + r;
}

double _Complex mul_complexd_val(double _Complex l, double _Complex r)
{
  return l * r;
}

double rotdist_complexd_val(double _Complex c, double r) {
  double _Complex x = c * (ctypes_compat_make_complex(cos(r), sin(r)));
  return fabs(ctypes_compat_creal(x)) + fabs(ctypes_compat_cimag(x));
}

float _Complex add_complexf_val(float _Complex l, float _Complex r)
{
  return l + r;
}

float _Complex mul_complexf_val(float _Complex l, float _Complex r)
{
  return l * r;
}

float rotdist_complexf_val(float _Complex c, float r) {
  float _Complex x = c * (ctypes_compat_make_complexf(cosf(r), sinf(r)));
  return fabsf(ctypes_compat_crealf(x)) + fabsf(ctypes_compat_cimagf(x));
}

static int (*global_stored_callback)(int) = NULL;

void store_callback(int (*callback)(int))
{
  global_stored_callback = callback;
}

int invoke_stored_callback(int x)
{
  return global_stored_callback(x);
}

vintfun *return_callback(vintfun *callback)
{
  return callback;
}

struct one_int return_struct_by_value(void)
{
  struct one_int v = { 3 };
  return v;
}

/* naive matrix operations */
void matrix_mul(int lrows, int lcols, int rcols,
                double *l, double *r, double *prod)
{
  int i, j, k;
  for (i = 0; i < lrows; i++) {
    for (j = 0; j < rcols; j++) {
      prod[i * rcols + j] = 0.0;
      for (k = 0; k < lcols; k++) {
        prod[i * rcols + j] += l[i * lcols + k] * r[k * rcols + j];
      }
    }
  }
}

double *matrix_transpose(int rows, int cols, double *matrix)
{
  int i, j;
  double *rv = malloc(rows * cols * sizeof *rv);

  for (i = 0; i < rows; i++)
    for (j = 0; j < cols; j++)
      rv[j * rows + i] = matrix[i * cols + j];

  return rv;
}

int (*plus_callback)(int) = NULL;

/* Sum the range [a, b] */
int sum_range_with_plus_callback(int a, int b)
{
  int sum = 0, i = 0;
  for (i = a; i <= b; i++) {
    sum += i;
  }
  return sum;
}

static callback_t *registered_callback = NULL;

void register_callback(callback_t *callback)
{
  registered_callback = callback;
}

void call_registered_callback(int times, int starting_value)
{
  int i;
  for (i = 0; i < times; i++) {
    int result = registered_callback();
    assert (result == starting_value++);
  }
}

#ifdef __APPLE__
#define sem_t dispatch_semaphore_t
#define sem_init(sem, sem_attr1, sem_init_value)                \
  ((*sem = dispatch_semaphore_create(sem_init_value)) == NULL)
#define sem_wait(sem)                                   \
  dispatch_semaphore_wait(*sem, DISPATCH_TIME_FOREVER)
#define sem_post(sem)                           \
  (dispatch_semaphore_signal(*sem),0)
#define sem_destroy(sem)                        \
  (dispatch_release(*sem),0)

#elif defined(_WIN32) && !defined(__CYGWIN__)
#define sem_t HANDLE
#define sem_init(sem, sem_attr1, sem_init_value)        \
  ((*sem = CreateSemaphore(NULL,0,32768,NULL)) == NULL)
#define sem_wait(sem) \
  (WAIT_OBJECT_0 != WaitForSingleObject(*sem,INFINITE))
#define sem_post(sem) (ReleaseSemaphore(*sem,1,NULL) == 0)
#define sem_destroy(sem) (CloseHandle(*sem) == 0 )
#endif

static sem_t semaphore1;
static sem_t semaphore2;
static int semaphores_intialized;

void initialize_waiters(void)
{
  if ( semaphores_intialized ) {
    assert ( sem_destroy(&semaphore1) == 0 );
    assert ( sem_destroy(&semaphore2) == 0 );
  }
  assert ( sem_init(&semaphore1, 0, 0) == 0 );
  assert ( sem_init(&semaphore2, 0, 0) == 0 );
  semaphores_intialized = 1;
}

void post1_wait2(void)
{
  int e;
  assert ( sem_post(&semaphore1) == 0 );
  errno = 0;
  do {
    e = sem_wait(&semaphore2);
  } while ( e && errno == EINTR );
  assert ( e == 0 );
}

void post2_wait1(void)
{
  int e;
  assert ( sem_post(&semaphore2) == 0 );
  errno = 0;
  do {
    e = sem_wait(&semaphore1);
  } while ( e && errno == EINTR );
  assert ( e == 0 );
}

size_t sizeof_s1(void) { return sizeof(struct s1); }
size_t alignmentof_s1(void) { return offsetof(struct { char c; struct s1 x; }, x); }
size_t offsetof_x1(void) { return offsetof(struct s1, x1); }
size_t offsetof_x2(void) { return offsetof(struct s1, x2); }
size_t offsetof_x3(void) { return offsetof(struct s1, x3); }
size_t offsetof_x4(void) { return offsetof(struct s1, x4); }
size_t sizeof_s2(void) { return sizeof(struct s2); }
size_t alignmentof_s2(void) { return offsetof(struct { char c; struct s2 x; }, x); }
size_t offsetof_y1(void) { return offsetof(struct s2, y1); }
size_t offsetof_y2(void) { return offsetof(struct s2, y2); }
size_t offsetof_y3(void) { return offsetof(struct s2, y3); }
size_t offsetof_y4(void) { return offsetof(struct s2, y4); }
size_t sizeof_s3(void) { return sizeof(struct s3); }
size_t alignmentof_s3(void) { return offsetof(struct { char c; struct s3 x; }, x); }
size_t offsetof_z1(void) { return offsetof(struct s3, z1); }
size_t offsetof_z2(void) { return offsetof(struct s3, z2); }
size_t sizeof_s4(void) { return sizeof(struct s4); }
size_t alignmentof_s4(void) { return offsetof(struct { char c; struct s4 x; }, x); }
size_t offsetof_z3(void) { return offsetof(struct s4, z3); }
size_t offsetof_z4(void) { return offsetof(struct s4, z4); }
size_t sizeof_s6(void) { return sizeof(s6); }
size_t alignmentof_s6(void) { return offsetof(struct { char c; s6 x; }, x); }
size_t offsetof_v1(void) { return offsetof(s6, v1); }
size_t offsetof_v2(void) { return offsetof(s6, v2); }

size_t sizeof_u1(void) { return sizeof(union u1); }
size_t alignmentof_u1(void) { return offsetof (struct { char c; union u1 x; }, x); }
size_t sizeof_u2(void) { return sizeof(u2); }
size_t alignmentof_u2(void) { return offsetof (struct { char c; u2 x; }, x); }

bool bool_and(bool l, bool r)
{
  return l && r;
}

int call_s5(struct s1 *s1, struct s5 *s5)
{
  return s5->w1(s1);
}

enum signed_enum classify_integer(int x)
{
  return (x < 0) ? minus_one : plus_one;
}

enum signed_enum out_of_range(void)
{
  return (enum signed_enum)2;
}

enum fruit next_fruit(enum fruit f)
{
  switch (f)
  {
  case Orange: return Apple;
  case Apple: return Banana;
  case Banana: return Pear;
  case Pear: return Orange;
  default: assert(0);
  }
}

int32_t sum_int_array(int32_t *arr, size_t len)
{
  int32_t sum = 0;
  size_t i = 0;
  for (; i < len; i++) {
    sum += arr[i];
  }
  return sum;
}

void *global_ocaml_value = NULL;

void save_ocaml_value(void *p)
{
  global_ocaml_value = p;
}
void *retrieve_ocaml_value(void)
{
  return global_ocaml_value;
}

int sixargs(int x1, int x2, int x3, int x4, int x5, int x6)
{
  return x1 + x2 + x3 + x4 + x5 + x6;
}

int return_10(void)
{
  return 10;
}

void return_void(int *x)
{
  *x = 10;
  return;
}

int callback_returns_char_a(char (*f)(void))
{
  return f() == 'a' ? 1 : 0;
}

#define GEN_RETURN_F(type)                          \
  type callback_returns_ ## type (type  (*f)(void)) \
  {                                                 \
    type x = f();                                   \
    return x;                                       \
  }                                                 \

GEN_RETURN_F(uint8_t)
GEN_RETURN_F(uint16_t)
GEN_RETURN_F(uint32_t)
GEN_RETURN_F(uint64_t)

GEN_RETURN_F(int8_t)
GEN_RETURN_F(int16_t)
GEN_RETURN_F(int32_t)
GEN_RETURN_F(int64_t)

GEN_RETURN_F(float)
GEN_RETURN_F(double)
GEN_RETURN_F(bool)

char *string_array[2] = { "Hello", "world" };
int32_t int_array[5] = { 0, 1, 2, 3, 4 };

void check_ones(const int *p, size_t sz)
{
  unsigned i = 0;
  for (; i < sz; i++) {
    assert (p[i] == 1);
  }
}

intnat max_caml_int(void)
{
  return (intnat)(((uintnat)-1) / 4);
}

static
uint64_t thread_id(void)
{
#ifdef _WIN32
  return (GetCurrentThreadId());
#else
  /* if pthread_t is a struct greater than uint64_t,
     the test could fail ... */
  union {
      uint64_t i;
      pthread_t t;
  } u;
  memset(&u, 0, sizeof(u));
  u.t = pthread_self();
  return u.i;
#endif
}

#ifndef _WIN32
typedef pthread_t thread_t;
typedef void *(*start_routine)(void *);
#else
typedef HANDLE thread_t;
typedef DWORD WINAPI (*start_routine)(void *);
#endif

static int thread_create(thread_t *t, start_routine f, void * param)
{
#ifndef _WIN32
  return (pthread_create(t, NULL, f, param));
#else
  HANDLE h = CreateThread(NULL,0,f,param,0,NULL);
  *t = h;
  return ( h == NULL );
#endif
}

static int thread_join(thread_t t)
{
#ifndef _WIN32
  return ( pthread_join(t, NULL) );
#else
  WaitForSingleObject(t, INFINITE);
  return 0;
#endif
}

typedef struct {
    void (*f)(uint64_t);
    const unsigned n_callback;
} t_info;

static void call_multiple_times_r(void *fp)
{
  const t_info * t = fp;
  void (*f)(uint64_t) = t->f;
  const unsigned n_callback = t->n_callback;
  const uint64_t tid = thread_id();
  unsigned i;
  for ( i = 0 ; i < n_callback ; ++i ) {
    f(tid);
  }
}

#ifndef _WIN32
static void *call_multiple_times(void *fp)
{
  call_multiple_times_r(fp);
  return NULL;
}
#else
static DWORD WINAPI call_multiple_times(void *fp)
{
  call_multiple_times_r(fp);
  return 0;
}
#endif

int foreign_thread_registration_test(void (*test_f)(uint64_t),
                                     unsigned n_threads,
                                     unsigned n_callback)
{
  thread_t * h_thread;
  unsigned i;
  int ret_code = 0;
  unsigned i_max = 0;
  t_info thread_info = { .f = test_f , .n_callback = n_callback };
  const uint64_t tid = thread_id();

  h_thread = malloc(n_threads * (sizeof *h_thread));
  if ( h_thread == NULL ){
    fputs("malloc failed\n",stderr);
    return 1;
  }

  for ( i = 0 ; i < n_threads ; ++i ) {
    if ( thread_create(&h_thread[i], call_multiple_times, &thread_info) ) {
      fputs("Error creating thread\n",stderr);
      ret_code = 1;
      break;
    }
  }
  i_max = i;

  for ( i = 0 ; i < i_max ; ++i ) {
    if ( i < n_callback ) {
      test_f(tid);
    }
    if ( thread_join(h_thread[i]) ) {
      fputs("Error joining thread\n",stderr);
      ret_code = 1;
    }
  }

  for ( i = n_threads ; i < n_callback ; ++i ) {
    test_f(tid);
  }

  free(h_thread);
  return ret_code;
}
