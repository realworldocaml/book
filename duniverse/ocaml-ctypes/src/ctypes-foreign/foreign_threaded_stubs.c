/*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <caml/mlvalues.h>
#include <caml/threads.h>

extern int (*ctypes_thread_register)(void);

#ifdef _WIN32
#include <caml/fail.h>
#include <windows.h>

/* The OCaml runtime stores the pointers to the information that must
   be cleaned up in thread local storage. Therefore
   caml_c_thread_unregister must be called from the thread itself.
   .CRT$XLA to .CRT$XLZ is an array of callback pointers that are
   called by the OS when the DLL is loaded and on thread attachment /
   detachment (they were introduced for the current task: set up and
   clean thread local storage).

   Note: Only Windows Vista and later execute the TLS callbacks for
   dynamically loaded DLLs (-> Windows XP restrictions: no toplevel
   support, bytecode executables must be compiled with '-custom').
*/

/* ctypes_tls_callback will be called for all threads. The OCaml
   runtime use the same TLS $index for its own threads and threads
   registered with caml_c_thread_register. TlsSetValue($index,NULL) is
   only called during caml_c_thread_unregister, but not for threads
   created by OCaml. Therefore an additional TLS index is allocated to
   ensure, that caml_c_thread_unregister is not called for these
   threads.
*/
static DWORD tls_index;
#define CTYPES_TLS_MAGIC_VALUE ((void*)0x78)

static void NTAPI
ctypes_tls_callback(void* a, DWORD reason, PVOID b)
{
  (void)a; (void)b;
  if ( reason == DLL_THREAD_DETACH ) {
    void * x =  TlsGetValue(tls_index);
    if ( x == CTYPES_TLS_MAGIC_VALUE ) {
      TlsSetValue(tls_index, NULL);
      caml_c_thread_unregister();
    }
  }
}

PIMAGE_TLS_CALLBACK __crt_ctypes_tls_callback__ __attribute__ \
 ((section(".CRT$XLB"))) = ctypes_tls_callback;

static int ctypes_thread_actually_register(void)
{
  int rv = caml_c_thread_register();
  if ( rv != 0 ) {
    /* errors ignored (like in the case of pthread_key_create).
       I can't raise an exception here and I can't store the information
       anywhere  */
    TlsSetValue(tls_index, CTYPES_TLS_MAGIC_VALUE);
  }
  return rv;
}

value ctypes_setup_thread_registration(value _)
{
  tls_index = TlsAlloc();
  if ( tls_index == TLS_OUT_OF_INDEXES ) {
    caml_failwith("ctypes_thread_registration: TlsAlloc failed");
  }
  ctypes_thread_register = ctypes_thread_actually_register;
  return Val_unit;
}
#else
#include <pthread.h>

static pthread_key_t cleanup_key;

static void ctypes_thread_unregister(void* _)
{
  caml_c_thread_unregister();
  pthread_setspecific(cleanup_key, NULL);
}

static int ctypes_thread_actually_register(void)
{
  int rv = caml_c_thread_register();

  if (rv != 0) {
    /* Register a destructor function for a TLS key that unregisters
       this thread from the OCaml runtime when the thread exits. */

    /* Assumption: caml_c_thread_unregister is not called in this
       thread, except by the destructor, so caml_c_thread_register()
       will always succeed.  Consequently, there is no need to protect
       the TLS-creation code with pthread_once.  (And at worst, if the
       assumption is violated then caml_c_thread_unregister will be
       called multiple times, which is harmless.) */ 
    pthread_key_create(&cleanup_key, ctypes_thread_unregister);
    pthread_setspecific(cleanup_key, &cleanup_key);
  }

  return rv;
}

value ctypes_setup_thread_registration(value _)
{
  ctypes_thread_register = ctypes_thread_actually_register;
  return Val_unit;
}
#endif
