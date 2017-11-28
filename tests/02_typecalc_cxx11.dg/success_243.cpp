/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

template <bool, typename> struct enable_if {};

template <typename T> struct enable_if<true, T> { typedef T type; };

template <typename T>
constexpr bool test()
{
    return sizeof(T) >= 4;
}

template <typename T>
struct A
{
    template <typename U = T,
             typename enable_if< test<U>(), bool >::type = true>
                 A(T) { }
};

A<int> a(3);
