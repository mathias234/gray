#include <cstdarg>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <ostream>
#include <new>


struct ValuePointer {
  void *value;
};


extern "C" {

ValuePointer value_from_f64(double value);

ValuePointer value_from_i64(int64_t value);

ValuePointer value_from_string(const char *str);

} // extern "C"
