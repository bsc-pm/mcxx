/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

void f(void)
{
    typedef int P;
    auto i = 3;
    typedef decltype(i) P;
}
