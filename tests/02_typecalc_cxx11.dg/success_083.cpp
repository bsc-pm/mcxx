/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
struct is_bind_expression;

template <typename T>
struct is_placeholder;

template<typename _Arg,
    bool _IsBindExp = is_bind_expression<_Arg>::value,
    bool _IsPlaceholder = (is_placeholder<_Arg>::value > 0)>
    class _Mu;

template<typename T>
class _Bind;

template <typename T>
T declval();

template <typename ...T>
struct tuple;

template<typename _Functor, typename... _Bound_args>
class _Bind<_Functor(_Bound_args...)>
{
    template<typename... _Args, typename _Result
        = decltype( declval<_Functor>()(
                    _Mu<_Bound_args>()( declval<_Bound_args&>(),
                        declval<tuple<_Args...>&>() )... ) )>
        _Result operator()();
};


