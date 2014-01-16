/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
void g(T, T);

template <typename T>
void f(T a, T b) noexcept(noexcept(g(a, b)))
{
}
