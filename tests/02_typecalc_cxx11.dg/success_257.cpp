/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
template < int N>
struct A
{};

namespace test1
{
    template < typename T>
    struct C
    {
        T** ppv = &pvar;
        T* pvar = &var;
        T var;
    };
}

int main() {
    test1::C<int> a1;
}
