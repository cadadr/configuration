/* gk-module.h --- C module helpers. */
#ifndef GK_MODULE_H
#define GK_MODULE_H

#include "emacs-module.h"

extern void
gk_bind_function (emacs_env *env, const char *name, emacs_value Sfun);

extern void
gk_provide (emacs_env *env, const char *feature);

/* Mandatory for all modules. */
int plugin_is_GPL_compatible;

#endif /* GK_MODULE_H */
