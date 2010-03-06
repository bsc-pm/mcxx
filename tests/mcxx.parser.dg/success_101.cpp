/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename _T>
struct A_base
{
    typedef _T* iterator;
};

template <typename _T>
struct A : A_base<_T>
{
    typedef A_base<_T> _Base;
    typedef typename _Base::iterator iterator;

    iterator fun(iterator);
};

template <typename _T>
typename A<_T>::iterator A<_T>::fun(iterator)
{
}
