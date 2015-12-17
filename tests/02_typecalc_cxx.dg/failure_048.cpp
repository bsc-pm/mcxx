/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/

typedef __typeof__("hola") T1;
T1 hola1 = "hola";
char *p1 = hola1;
