/*
<testinfo>
test_generator=config/mercurium-fe-only
test_CXXFLAGS="-std=c++11"
test_compile_fail=yes
</testinfo>
*/
struct A
{
};

struct B : A
{
    template <typename T>
    void foo() override { }
};

