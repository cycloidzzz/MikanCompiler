#include "runtime.h"

int64_t read_int() {
  int64_t number;
  scanf("%lld", &number);
  return number;
}

void print_int(int64_t number) { printf("%lld", number); }

void println_int(int64_t number) { printf("%lld\n", number); }