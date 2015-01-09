/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
typedef unsigned long size_t;

template<typename _Tp>
struct __add_ref
{ typedef _Tp& type; };

template<typename _Tp>
struct __add_ref<_Tp&>
{ typedef _Tp& type; };

template<size_t _Idx, typename... _Elements>
struct _Tuple_impl;

template<size_t _Idx>
struct _Tuple_impl<_Idx>
{
};

template<size_t _Idx, typename _Head, typename... _Tail>
struct _Tuple_impl<_Idx, _Head, _Tail...>
: public _Tuple_impl<_Idx + 1, _Tail...>
{
};

template<typename... _Elements>
class tuple;

template<size_t __i, typename _Tp>
struct tuple_element;

template<size_t __i, typename _Head, typename... _Tail>
struct tuple_element<__i, tuple<_Head, _Tail...> >
: tuple_element<__i - 1, tuple<_Tail...> > { };

template<typename _Head, typename... _Tail>
struct tuple_element<0, tuple<_Head, _Tail...> >
{
    typedef _Head type;
};

template<size_t __i, typename _Head, typename... _Tail>
typename __add_ref<_Head>::type
__get_helper(_Tuple_impl<__i, _Head, _Tail...>& __t) noexcept;

template<size_t __i, typename... _Elements>
typename __add_ref<
typename tuple_element<__i, tuple<_Elements...>>::type
>::type
get(tuple<_Elements...>& __t) noexcept
{ return __get_helper<__i>(__t); }

template<typename... _Elements>
class tuple : public _Tuple_impl<0, _Elements...>
{
};

tuple<int*, float*, double*> d;
void foo(void)
{
    {
        int*& ri = __get_helper<0>(d);
        float*& rf = __get_helper<1>(d);
        double*& rd = __get_helper<2>(d);
    }

    {
        int*& ri = get<0>(d);
        float*& rf = get<1>(d);
        double*& rd = get<2>(d);
    }
}
