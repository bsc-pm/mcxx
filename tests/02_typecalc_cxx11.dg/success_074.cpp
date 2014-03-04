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

template<class _Tp>
struct A
{
    template <typename ...T, std::size_t ...Index>
        void f();
};

template<class _Tp>
template <typename ...T, std::size_t ...Index>
void A<_Tp>::f()
{
    // Mercurium attempted to instantiate this
    typename C<Index + 1 ...>::K k;
}
