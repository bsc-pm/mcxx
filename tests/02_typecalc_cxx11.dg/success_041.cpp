/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T, typename S>
void f(T t1, S s2)
{
    [](int x) { return x + 1; }(1, 2);
}
