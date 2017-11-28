/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

template <typename T1, T1 ...Is>
struct integer_sequence { };

template <int ...Is1>
using index_sequence = integer_sequence<int, Is1...>;

template <typename T2>
struct A
{
    constexpr A()
        : A(index_sequence<1, 2>()) { }

    template <int ...Is2>
    constexpr A(index_sequence<Is2...>) { }
};

A<int> a;
