/*
<testinfo>
test_generator="config/mercurium-cxx11"
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
