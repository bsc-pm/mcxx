/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
template <typename T>
void f(T t)
{
    for (auto x : t)
    {
        x = 1;
    }
}
