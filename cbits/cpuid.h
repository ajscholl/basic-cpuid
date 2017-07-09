#ifndef CPUID_H
#define CPUID_H

#include <stdint.h>

void call_cpuid(uint32_t eax, uint32_t ecx, uint32_t *result);
uint64_t call_xgetbv(uint32_t ecx);

#endif
