/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

namespace std
{
    typedef unsigned long size_t;
}

template <std::size_t ...Index>
struct C { };

template <std::size_t ...Index>
void f()
{
    C<Index + 1 ...> c;
}
