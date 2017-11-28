/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
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

struct S
{
    char c[4];
} s;

void bar()
{
    foo(s);
}
