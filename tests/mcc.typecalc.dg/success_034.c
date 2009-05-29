void f(void)
{
     __attribute__((altivec(vector__))) float zero_float = (__attribute__((altivec(vector__))) float) {0.0, 0.0, 0.0, 0.0};
     // Seems that this is not valid in Altivec
     // __attribute__((altivec(vector__))) double zero_double = (__attribute__((altivec(vector__))) double) {0.0, 0.0, 0.0, 0.0};
}
