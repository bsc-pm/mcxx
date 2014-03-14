/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <bool B>
struct enable_if;

template <typename ...T>
struct __and_;

template<typename... _Types>
struct _Pack;

// _Require
template<typename... _Cond>
using _Require = typename enable_if<__and_<_Cond...>::value>::type;

// _AllConvertible
template<typename _From, typename _To, bool B = _From::value == _To::value>
struct _AllConvertible;

// _Mem_fn
template<typename _MemberPointer>
class _Mem_fn;

template<typename _Res, typename _Class, typename... _ArgTypes>
class _Mem_fn<_Res (_Class::*)(_ArgTypes...)>
{
    template<typename... _Args>
        using _RequireValidArgs
        = _Require<_AllConvertible<_Pack<_Args...>, _Pack<_ArgTypes...>>>;
};
