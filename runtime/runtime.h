#ifndef MIKANCOMPILER_RUNTIME_RUNTIME_H
#define MIKANCOMPILER_RUNTIME_RUNTIME_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

extern int64_t read_int() __asm__("read_int");
extern void print_int(int64_t number) __asm__("print_int");
extern void print_int_ln(int64_t number) __asm__("print_int_ln");

#endif  // ! MIKANCOMPILER_RUNTIME_RUNTIME_H