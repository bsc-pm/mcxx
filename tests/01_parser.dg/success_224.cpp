/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    typedef int (A::*Pf1)(int);
    typedef int (::A<T>::*Pf2)(int);
    typedef int (::A<T>::template A<T>::*Pf3)(int);
};
