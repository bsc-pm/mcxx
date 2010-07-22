/*
<testinfo>
test_generator=config/mercurium
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
