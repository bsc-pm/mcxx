/*
<testinfo>
test_generator="config/mercurium"
test_CXXFLAGS="-std=c++11"
</testinfo>
*/

template <typename ...T, T ...N>
void f()
{
}

void g()
{
    f<int, float>();
}
