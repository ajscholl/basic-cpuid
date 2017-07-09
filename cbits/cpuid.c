
#include "cpuid.h"

void call_cpuid(uint32_t eax, uint32_t ecx, uint32_t *result) {
#if defined(__x86_64__) || defined(__i386__)
    __asm__ __volatile__(
		    "cpuid\r\n"
		    "mov %%eax, %0\r\n"
		    "mov %%ebx, %1\r\n"
		    "mov %%ecx, %2\r\n"
		    "mov %%edx, %3\r\n"
		    : "=g" (result[0]), "=g" (result[1]), "=g" (result[2]), "=g" (result[3])
		    : "a" (eax), "c" (ecx)
		    : "memory", "ebx", "edx", "cc"
		);
#else
    result[0] = 0;
    result[1] = 0;
    result[2] = 0;
    result[3] = 0;
#endif
}

uint64_t call_xgetbv(uint32_t ecx) {
#if defined(__x86_64__) || defined(__i386__)
    uint64_t rax;
    __asm__ __volatile__(
            "xgetbv\r\n"
            "shl $32, %%rdx \r\n"
            "or %%rdx, %%rax \r\n"
            : "=a" (rax)
            : "c" (ecx)
            : "rdx", "cc"
        );
    return rax;
#else
    return 0;
#endif
}
