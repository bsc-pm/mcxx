/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename T>
struct C
{
    template <typename S, typename Q = S*>
        struct A
        {
        };
};

template <typename T>
struct D
{
    typename C<T>::template A<T> c;
};

D<int> d;
