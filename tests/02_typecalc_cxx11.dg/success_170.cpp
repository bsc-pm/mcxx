/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
typedef decltype(sizeof(int)) size_t;

template<size_t... _Indexes>
struct _Index_tuple
{
    typedef _Index_tuple<_Indexes..., sizeof...(_Indexes)> __next;
};

template<size_t _Num>
struct _Build_index_tuple
{
    typedef typename _Build_index_tuple<_Num - 1>::__type::__next __type;
};

template<>
struct _Build_index_tuple<0>
{
    typedef _Index_tuple<> __type;
};

template<typename _Signature>
struct _Bind;

template<typename _Functor, typename... _Bound_args>
struct _Bind<_Functor(_Bound_args...)>
{
    typedef typename _Build_index_tuple<sizeof...(_Bound_args)>::__type _Bound_indexes;
};

void f(void)
{
    _Bind<int(float, double)> a;
}
