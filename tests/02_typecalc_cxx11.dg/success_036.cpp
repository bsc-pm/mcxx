/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
struct true_type { };
struct false_type { };

template<typename T>
struct add_rvalue_reference
{
    typedef T&& type;
};

template<typename T>
struct add_rvalue_reference<T&> { };

template<>
struct add_rvalue_reference<void> { };

template<typename _Tp>
typename add_rvalue_reference<_Tp>::type declval() noexcept;

struct __do_is_nary_constructible_impl
{
    template<typename _Tp, typename... _Args, typename = decltype(_Tp(declval<_Args>()...))>
        static true_type __test(int);

    template<typename, typename...>
        static false_type __test(...);
};

template<typename _Tp, typename... _Args>
struct __is_nary_constructible_impl : public __do_is_nary_constructible_impl
{
    typedef decltype(__test<_Tp, _Args...>(0)) type;
};

struct B
{
    B(int*, float*);
};

typedef struct true_type T1;
typedef __is_nary_constructible_impl<B, int*, float*>::type T1;

typedef struct false_type T2;
typedef __is_nary_constructible_impl<B, int*, int*>::type T2;
