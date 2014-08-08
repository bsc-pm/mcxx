/*
<testinfo>
test_generator=config/mercurium-cxx11
test_compile_fail=yes
</testinfo>
*/

template <typename ...S>
void f(S ...);


void g()
{
    f<int, float>(1, 2.3f, 'a');
}
