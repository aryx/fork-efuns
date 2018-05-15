#include <stdlib.h>
#include <limits.h>

#ifdef _PLAN9_SOURCE
#include <lib9.h>
#define PATH_MAX _POSIX_PATH_MAX 
#include <string.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>


#define Val_none Val_int(0)


static value
Val_some( value v )
{
  CAMLparam1( v );
  CAMLlocal1( some );
  some = alloc(1, 0);
  Store_field( some, 0, v );
  CAMLreturn( some );
}

value
caml_realpath(value v) {
  char *input;
  char output[PATH_MAX];
  char *result;

  CAMLparam1(v);

  input = String_val(v);

#ifdef _PLAN9_SOURCE
  strncpy(output, input, PATH_MAX);
  result = cleanname(output);
#else
  result = realpath(input, output);
#endif

  if (result == NULL) {
    CAMLreturn(Val_none);
  } else {
    CAMLreturn(Val_some(copy_string(output)));
  }
}
