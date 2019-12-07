/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef TEST_FUNCTIONS_H
#define TEST_FUNCTIONS_H

#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>

#include <caml/mlvalues.h>

typedef int intfun(int, int);
extern int higher_order_1(intfun *, int, int);
typedef int acceptor(intfun *, int, int);
extern int higher_order_3(acceptor *, intfun *, int, int);
typedef int vintfun(int);
extern int higher_order_simplest(vintfun *);
extern intfun *returning_funptr(int);
extern int accepting_possibly_null_funptr(intfun *, int, int);
extern int global;
extern int *return_global_address(void);
extern double float_pointer_callback(void (*)(double *), double);
extern int write_through_callback(int (*)(int *));
extern int write_through_callback_pointer_pointer(int (*)(int **, int *));
extern int is_null(void *);
extern int callback_returns_funptr(vintfun *(*)(int), int);
extern int *pass_pointer_through(int *, int *, int);
struct simple {
  int i;
  double f;
  struct simple *self;
};

extern int accept_struct(struct simple);
extern struct simple return_struct(void);
union padded {
  int64_t i;
  char    a[sizeof(int64_t) + 1];
};
extern int64_t sum_union_components(union padded *, size_t);
extern union padded add_unions(union padded, union padded);

extern void concat_strings(const char **, int, char *);

union number {
  int i;
  double d;
};

struct tagged {
  char tag;
  union number num;
};

extern struct tagged add_tagged_numbers(struct tagged, struct tagged);

extern double accepts_pointer_to_array_of_structs(struct tagged(*)[5]);
#define GLOBAL_STRING "global string"
struct global_struct {
  size_t len;
  const char str[sizeof GLOBAL_STRING];
};

extern struct global_struct global_struct;
struct triple {
  double elements[3];
};
extern struct triple add_triples(struct triple, struct triple);
struct animal;
struct chorse;
extern int check_name(struct animal *, char *);
extern char *chorse_colour(struct chorse *);
extern char *chorse_say(struct animal *);
extern char *chorse_identify(struct animal *);
extern struct chorse *new_chorse(int);
extern int accept_pointers(float *,
                           double *,
                           short *,
                           int *,
                           long *,
                           long long *,
                           intnat *,
                           int8_t *,
                           int16_t *,
                           int32_t *,
                           int64_t *,
                           uint8_t *,
                           uint16_t *,
                           uint32_t *,
                           uint64_t *,
                           size_t *,
                           unsigned short *,
                           unsigned *,
                           unsigned long *,
                           unsigned long long *);
int accept_pointers_to_pointers(int *, int **, int ***, int ****);
intfun **returning_pointer_to_function_pointer(void);
int accepting_pointer_to_function_pointer(intfun **);
typedef int pintfun1(int *, int *);
int passing_pointers_to_callback(pintfun1 *);
typedef int *pintfun2(int, int);
int accepting_pointer_from_callback(pintfun2 *);
signed char retrieve_SCHAR_MIN(void);
signed char retrieve_SCHAR_MAX(void);
unsigned char retrieve_UCHAR_MAX(void);
char retrieve_CHAR_MIN(void);
char retrieve_CHAR_MAX(void);
short retrieve_SHRT_MIN(void);
short retrieve_SHRT_MAX(void);
unsigned short retrieve_USHRT_MAX(void);
int retrieve_INT_MIN(void);
int retrieve_INT_MAX(void);
unsigned int retrieve_UINT_MAX(void);
long retrieve_LONG_MAX(void);
long retrieve_LONG_MIN(void);
unsigned long retrieve_ULONG_MAX(void);
long long retrieve_LLONG_MAX(void);
long long retrieve_LLONG_MIN(void);
unsigned long long retrieve_ULLONG_MAX(void);
int8_t retrieve_INT8_MIN(void);
int16_t retrieve_INT16_MIN(void);
int32_t retrieve_INT32_MIN(void);
int64_t retrieve_INT64_MIN(void);
int8_t retrieve_INT8_MAX(void);
int16_t retrieve_INT16_MAX(void);
int32_t retrieve_INT32_MAX(void);
int64_t retrieve_INT64_MAX(void);
uint8_t retrieve_UINT8_MAX(void);
uint16_t retrieve_UINT16_MAX(void);
uint32_t retrieve_UINT32_MAX(void);
uint64_t retrieve_UINT64_MAX(void);
size_t retrieve_SIZE_MAX(void);
float retrieve_FLT_MIN(void);
float retrieve_FLT_MAX(void);
double retrieve_DBL_MIN(void);
double retrieve_DBL_MAX(void);
void add_complexd(double _Complex *, double _Complex *, double _Complex *);
void mul_complexd(double _Complex *, double _Complex *, double _Complex *);
void rotdist_complexd(double _Complex *, double *, double *);
void add_complexld(long double _Complex *, long double _Complex *, long double _Complex *);
void mul_complexld(long double _Complex *, long double _Complex *, long double _Complex *);
void rotdist_complexld(long double _Complex *, long double *, long double *);
void add_complexf(float _Complex *, float _Complex *, float _Complex *);
void mul_complexf(float _Complex *, float _Complex *, float _Complex *);
void rotdist_complexf(float _Complex *, float *, float *);
double _Complex add_complexd_val(double _Complex, double _Complex);
double _Complex mul_complexd_val(double _Complex, double _Complex);
double rotdist_complexd_val(double _Complex, double);
long double _Complex add_complexld_val(long double _Complex, long double _Complex);
long double _Complex mul_complexld_val(long double _Complex, long double _Complex);
long double rotdist_complexld_val(long double _Complex, long double);
float _Complex add_complexf_val(float _Complex, float _Complex);
float _Complex mul_complexf_val(float _Complex, float _Complex);
float rotdist_complexf_val(float _Complex, float);
void store_callback(int (*callback)(int));
int invoke_stored_callback(int);
vintfun *return_callback(vintfun *);
struct one_int { int i; };
struct one_int return_struct_by_value(void);
void matrix_mul(int, int, int, double *, double *, double *);
double *matrix_transpose(int, int, double *);
int (*plus_callback)(int);
int sum_range_with_plus_callback(int, int);
typedef int callback_t(void);
void register_callback(callback_t *);
void call_registered_callback(int, int);
void initialize_waiters(void);
void post1_wait2(void);
void post2_wait1(void);

struct s1 { int x1, x2, x3, x4; };
struct s2 { int y1, y2, y3, y4; };
struct s3 { int z1; struct s3 *z2; };
struct s4 { struct s3 z3; struct s3 *z4; };
struct s5 { int (*w1)(struct s1 *); };
typedef struct { int v1; float v2; } s6;

size_t sizeof_s1(void);
size_t alignmentof_s1(void);
size_t offsetof_x1(void);
size_t offsetof_x2(void);
size_t offsetof_x3(void);
size_t offsetof_x4(void);
size_t sizeof_s2(void);
size_t alignmentof_s2(void);
size_t offsetof_y1(void);
size_t offsetof_y2(void);
size_t offsetof_y3(void);
size_t offsetof_y4(void);

size_t sizeof_s3(void);
size_t alignmentof_s3(void);
size_t offsetof_z1(void);
size_t offsetof_z2(void);

size_t sizeof_s4(void);
size_t alignmentof_s4(void);
size_t offsetof_z3(void);
size_t offsetof_z4(void);

size_t sizeof_s6(void);
size_t alignmentof_s6(void);
size_t offsetof_v1(void);
size_t offsetof_v2(void);

union u1 { char x1; float x2; double x3; char x4[13]; };
typedef union { int t1; float t2; } u2;

size_t sizeof_u1(void);
size_t alignmentof_u1(void);

size_t sizeof_u2(void);
size_t alignmentof_u2(void);

bool bool_and(bool, bool);
int call_s5(struct s1 *, struct s5 *);

enum letter { A, B, C = 10, D };

enum fruit { Orange, Apple, Banana, Pear };
enum bears { Edward, Winnie, Paddington };
enum signed_enum { minus_one = -1, plus_one = 1 };

enum fruit next_fruit(enum fruit);
enum signed_enum classify_integer(int);
enum signed_enum out_of_range(void);

struct fruit_cell {
  enum fruit frt;
  struct fruit_cell *next;
};

typedef enum letter letter_t;
typedef enum bears bears_t;

int32_t sum_int_array(int32_t *, size_t);

void save_ocaml_value(void *);
void *retrieve_ocaml_value(void);

int sixargs(int, int, int, int, int, int);
int return_10(void);
void return_void(int *);

int callback_returns_char_a(char (*)(void));

uint8_t callback_returns_uint8_t(uint8_t (*f)(void));
uint16_t callback_returns_uint16_t(uint16_t (*f)(void));
uint32_t callback_returns_uint32_t(uint32_t (*f)(void));
uint64_t callback_returns_uint64_t(uint64_t (*f)(void));

int8_t callback_returns_int8_t(int8_t (*f)(void));
int16_t callback_returns_int16_t(int16_t (*f)(void));
int32_t callback_returns_int32_t(int32_t (*f)(void));
int64_t callback_returns_int64_t(int64_t (*f)(void));

float callback_returns_float(float (*f)(void));
double callback_returns_double(double (*f)(void));
bool callback_returns_bool(bool (*f)(void));

extern char *string_array[2];
extern int32_t int_array[5];

void check_ones(const int *, size_t);

intnat max_caml_int(void);

int foreign_thread_registration_test(void (*)(uint64_t),unsigned,unsigned);
#endif /* TEST_FUNCTIONS_H */
