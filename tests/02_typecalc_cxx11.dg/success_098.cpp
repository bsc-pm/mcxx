/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
class my_function;

template<typename>
  struct add_rvalue_reference;
template<typename _Tp>
  typename add_rvalue_reference<_Tp>::type declval() noexcept;

template<typename T1, typename T2>
struct __or_;

template<typename _Tp>
struct is_void;

template<typename _From, typename _To>
struct is_convertible;

template<typename _From, typename _To>
using __check_func_return_type
= __or_<is_void<_To>, is_convertible<_From, _To>>;

template<bool, typename _Tp = void>
struct enable_if;

template<typename _Res, typename ... _ArgTypes>
class my_function<_Res(_ArgTypes...)>
{
    typedef _Res _Signature_type(_ArgTypes...);
    template<typename _Functor>
        using _Invoke = decltype(__callable_functor(declval<_Functor&>())
                (declval<_ArgTypes>()...));
    template<typename _Functor>
        using _Callable = __check_func_return_type<_Invoke<_Functor>, _Res>;
    template<typename _Cond, typename _Tp>
        using _Requires = typename enable_if<_Cond::value, _Tp>::type;

    template<typename _Functor,
        typename = _Requires<_Callable<_Functor>, void>>
            my_function(_Functor);
};
