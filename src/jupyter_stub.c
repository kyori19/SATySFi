#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

#define EXT_FUNC(name, argdefs, args) \
  void name argdefs { \
    static void (*name) argdefs = NULL; \
    if (name == NULL) { \
      name = get_ext_func(#name); \
    } \
    name args; \
  }

typedef void *value;

void *get_ext_func(char *func_name) {
  void *handle = dlopen("libsatysfi-jupyter-ext.so", RTLD_LAZY);
  if (!handle) {
    fprintf(stderr, "cannot find Jupyter Kernel extension: %s\n", dlerror());
    exit(1);
  }

  void *func = dlsym(handle, func_name);
  if (!func) {
    fprintf(stderr, "invalid Jupyter Kernel extension: %s\n", dlerror());
    exit(1);
  }

  return func;
}

EXT_FUNC(start_kernel, (value connection_file), (connection_file))
EXT_FUNC(return_text, (value execution_count, value data), (execution_count, data))
EXT_FUNC(return_pdf, (value execution_count, value data, value rasterize), (execution_count, data, rasterize))
EXT_FUNC(return_error, (value ename, value evalue), (ename, evalue))
