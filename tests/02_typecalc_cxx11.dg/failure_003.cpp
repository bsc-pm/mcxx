/*
<testinfo>
test_generator="config/mercurium-fe-only"
test_CXXFLAGS="-std=c++11"
test_compile_fail=yes
</testinfo>
*/

template < typename T>
auto f(T a) -> typename decltype(a)::MyType
{}

int main()
{
    int x;
    f(x);
}
