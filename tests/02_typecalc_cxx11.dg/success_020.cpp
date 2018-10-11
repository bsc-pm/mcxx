/*
<testinfo>
test_generator="config/mercurium-cxx11"

# This test fails in g++ 4.8 but it is supposed to work in the future

# This combination of flags is a bit awkward but works.
# This test is not going to generate a binary but a file that contains
# the generated code of Mercurium. The 'export test_nolink=no' is used to remove the '-c' flag.

export test_nolink=no
test_noexec=yes
test_CXXFLAGS="-y"
</testinfo>
*/

template <typename R1, typename R2>
struct W1 { };

template <template <typename, typename> class W2>
struct A { };

template <template <typename, typename> class ...W3>
struct B { };

template <template <typename, typename> class ...W3>
void f(A<W3...> &a, B<W3...> &b);

void g()
{
    A<W1> a;
    B<W1> b;

    ::f(a, b);
}
