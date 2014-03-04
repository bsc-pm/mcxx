/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
template <unsigned long>
struct B;

template <typename T>
void f()
{
    constexpr unsigned long s = T::f();

    B<s>::G();
}
