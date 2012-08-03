/*
<testinfo>
test_generator=config/mercurium
test_compile_fail=yes
</testinfo>
*/

// Note that in the case
// void f(int);
// void f(double);
//
// f(0.0f) would end calling f(double)
//
// but gcc does not seem to extend this behaviour to complex types

void cf(_Complex int);
void cf(_Complex double);

void g()
{

    cf(0.0f);
}
