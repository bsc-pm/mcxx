/*
<testinfo>
test_generator="config/mercurium-fe-only"
test_CXXFLAGS="-std=c++11"
test_compile_fail=yes
</testinfo>
*/

typedef decltype("hola") T1;
T1 hola1 = "hola";
char *p1 = hola1;
