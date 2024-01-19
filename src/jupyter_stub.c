#include <dlfcn.h>
#include <stdio.h>

typedef void *value;

void *ext_func(char *func_name) {
  void *handle = dlopen("libsatysfi-jupyter-ext.so", RTLD_LAZY);
  if (!handle) {
    printf("Error: %s\n", dlerror());
    exit(1);
  }

  void *func = dlsym(handle, func_name);
  if (!func) {
    printf("Error: %s\n", dlerror());
    exit(1);
  }

  return func;
}

void start_kernel(value file_name) {
  void (*start_kernel)(value) = ext_func("start_kernel");
  start_kernel(file_name);
}

void return_text(value execution_count, value data) {
  void (*return_text)(value, value) = ext_func("return_text");
  return_text(execution_count, data);
}

void return_pdf(value execution_count, value data, value rasterize) {
  void (*return_pdf)(value, value, value) = ext_func("return_pdf");
  return_pdf(execution_count, data, rasterize);
}

void return_error_message(value ename, value evalue, value trace) {
  void (*return_error_message)(value, value, value) = ext_func("return_error_message");
  return_error_message(ename, evalue, trace);
}
