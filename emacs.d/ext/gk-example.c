/* gk-example.c --- example C module.

   A test of the new Emacs C FFI. */

/* The API. */
#include "gk-module.h"

#include <string.h>

/* New emacs lisp function. All function exposed to Emacs must have
   this prototype. */
static emacs_value
gk_example_hello (emacs_env *env, int nargs, emacs_value args[], void *data)
{
  /* Say hello. */
  const char *str = "Hello from gk-module.so!";
  emacs_value fargs[1];
  emacs_value Qmessage = env->intern (env, "message");
  fargs[0] = env->make_string (env, str, strlen(str));
  env->funcall (env, Qmessage, 1, fargs);
  return env->intern (env, "t");
}

/* The entry point to the module. */
int emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);

  emacs_value hello
    = env->make_function (env, 0, 0, gk_example_hello, "Says hello.", NULL);

  gk_bind_function (env, "gk-example-hello", hello);
  gk_provide (env, "gk-example");

  /* Loaded successfully. */
  return 0;
}
