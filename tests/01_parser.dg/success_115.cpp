/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename E>
struct A
{
    typedef int size_type;

    static const size_type npos;
};

template <typename E1>
const typename A<E1>::size_type A<E1>::npos = (typename A<E1>::size_type)(-1);
