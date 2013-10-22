/*
<testinfo>
test_generator="config/mercurium"
test_CXXFLAGS="-std=c++11"
</testinfo>
*/

void f(void)
{
    typedef int P;
    auto i = 3;
    typedef decltype(i) P;
}
