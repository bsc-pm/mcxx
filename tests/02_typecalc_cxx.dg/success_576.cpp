/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct A
{
};

struct C
{
    C(A);
};

struct B : A
{
};

void f(C);

void g(const B& b)
{
    f(b);
}
