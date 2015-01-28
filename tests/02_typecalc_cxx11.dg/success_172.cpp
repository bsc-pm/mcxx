/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
template<typename _Signature>
class result_of;

template<typename _Functor, typename... _Bound_args>
void f()
{
    _Functor a(typename result_of<int(_Bound_args)>::type...);
}

