#include <cstdarg>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>


struct NativeFunction;

struct InterpreterPointer {
  void *value;
};

struct ValuePointer {
  void *value;
};


extern "C" {

void declare_function(NativeFunction native_function);

InterpreterPointer interpreter_load_file(const char *name);

void interpreter_run(InterpreterPointer pointer);

ValuePointer value_from_f64(double value);

ValuePointer value_from_i64(int64_t value);

ValuePointer value_from_string(const char *str);

} // extern "C"
