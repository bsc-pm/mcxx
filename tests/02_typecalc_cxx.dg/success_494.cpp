/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct B { };

template <typename T>
struct E {
    typedef T** Type;
};

template <typename T1, typename T2>
struct A1
{
    typedef B<int> C;

    typedef typename E<C>::Type A_Type;
};


template <typename T1, typename T2>
struct A2
{
    typedef B<int> D;

    typedef typename E<D>::Type A_Type;
};
