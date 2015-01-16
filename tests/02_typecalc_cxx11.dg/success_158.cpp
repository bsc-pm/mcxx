/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct false_type { };
struct true_type { };

template <typename _Check, typename T = void>
struct enable_if
{
};

template <typename T>
struct enable_if<true_type, T>
{
    typedef T type;
};

template <typename T>
using require = typename enable_if<T>::type;

template <typename _Type>
struct allocator {
    allocator(_Type);
};

template <typename _Alloc>
struct allocator_traits
{
    template<typename _Tp, typename... _Args>
        struct __construct_helper
        {
            template<typename _Alloc2,
              typename = decltype( _Alloc2( _Args()... ) ) >
              static true_type __test(int);

            template<typename>
                static false_type __test(...);

            using type = decltype(__test<_Alloc>(0));
        };

    template<typename _Tp, typename... _Args>
        using __has_construct
        = typename __construct_helper<_Tp, _Args...>::type;

    template<typename _Tp, typename... _Args>
        static require<__has_construct<_Tp, _Args...>> foo() { }
};

void f(void)
{
    allocator<int> *a;
    typedef allocator<int> A;

    allocator_traits< allocator<int> >::foo<int, int>();
}
