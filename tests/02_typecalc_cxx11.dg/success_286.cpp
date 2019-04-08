/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
template < typename T >
struct C
{
    constexpr static int value = 1;
};

template < typename T>
void foo() noexcept(C<T>::value) {}


template <>
void foo<int>() noexcept(C<int>::value)
{
}
