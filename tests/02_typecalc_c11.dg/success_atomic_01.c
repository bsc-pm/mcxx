/*
<testinfo>
test_generator="config/mercurium-c11"
</testinfo>
*/

#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 9)
_Atomic int x0 = 0;
_Atomic signed int x1 = 0;
_Atomic unsigned int x2 = 0;

_Atomic long x3 = 0;
_Atomic signed long x4 = 0;
_Atomic unsigned long x5 = 0;

_Atomic short x6 = 0;
_Atomic signed short x7 = 0;
_Atomic unsigned short x8 = 0;

_Atomic _Bool x9 = 0;

_Atomic char x10 = 0;
_Atomic signed char x11 = 0;
_Atomic unsigned char x12 = 0;
#endif
