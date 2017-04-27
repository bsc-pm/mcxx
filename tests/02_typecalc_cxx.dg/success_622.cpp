/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct K
{
   struct impl {};
};

template < typename T>
struct C
{
    typedef T data;
};

template <typename T>
struct D : public C<T> {

    typedef typename C<T>::data::impl dummy0;
    typedef typename D::data::impl dummy1;
    typedef typename D<T>::data::impl dummy2;
    typedef typename D<T>::template C<T>::data::impl dummy3;

    dummy0 v0;
    dummy1 v1;
    dummy2 v2;
    dummy3 v3;
};

D<K> var;
