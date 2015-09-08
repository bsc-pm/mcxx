/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
    const int &foo() const;
};

auto A::foo() const -> const int&
{
}
