/*
<testinfo>
test_generator=config/mercurium-fe-only
test_CXXFLAGS="-std=c++11"
test_compile_fail=yes
</testinfo>
*/
template<bool, typename _Tp = void>
struct enable_if
{ };


template<typename _Tp>
struct enable_if<true, _Tp>
{ typedef _Tp type; };

template <typename T,
         typename
        enable_if<sizeof(T) != 1, bool>::type = true>
        void foo(T t);


void bar()
{
    foo('c');
}
