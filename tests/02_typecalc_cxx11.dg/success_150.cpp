/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename ...S>
void f(S ...);


void g()
{
    f<int, float>(1, 2.3f, 'a');
}
