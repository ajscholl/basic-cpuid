# The basic-cpuid package

Similar to the cpuid package this package allows one to
call the CPUID instruction from plain Haskell. In contrast
to the cpuid package it is very minimalistic and provides
almost no additional helper functions. It is possible to
build this package on every platform. If the platform does
not support the CPUID instruction, calling it from Haskell
will just throw an exception.

It additionally supports the XGETBV instruction as well and
provides some utility functions to detect SSE2, AVX2 and
AVX512f support.
