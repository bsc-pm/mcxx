/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
struct A
{
    A();
};

template <typename T>
A<T>::A() = default;
