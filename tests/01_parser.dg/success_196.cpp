/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
void f();
};

template <typename T>
inline void A<T>::f()
{
}

template <> void A<int>::f();
template <> void A<float>::f();
