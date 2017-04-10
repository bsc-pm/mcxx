/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
namespace M
{
    struct A
    { };
    void foo(void f(A)) {}
}

void my_fun(M::A var)
{}

int main()
{
    foo(my_fun);
}

