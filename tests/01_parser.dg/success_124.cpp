/*
<testinfo>
test_generator=config/mercurium

# This test is faulty because the implicit type
# of member functions of the bases pulled in by
# using declarations do not have their implicit
# member assumed to be of the derived class
# See ticket #337
test_compile_faulty=yes
test_compile_fail=yes
</testinfo>
*/
struct A
{
    template <typename _T>
    int* foo(_T);
};

struct B : A
{
    using A::foo;

    template <typename _T>
    float* foo(_T);
};

int main(int argc, char* argv[])
{
    B b;
    b.foo(3);
}
