/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/
#include <assert.h>
#include <vector>

struct A {

    std::vector<int> v;
    A(int n) : v(n, 40) { }

    template<int D>
    void foo();

    void bar(int& x, int N)
    {
        x += N;
    }
};

template<int D>
void A::foo()
{
    for (std::vector<int>::iterator it = v.begin(); it != v.end(); ++it)
    {
#pragma omp task
        {
            bar(*it, D);
        }
    }
#pragma omp taskwait
}

int main(int argc, char *argv[])
{
    A a(100);

    a.foo<2>();

    for (std::vector<int>::iterator it = a.v.begin(); it != a.v.end(); ++it)
    {
        assert(*it == 42);
    }

    return 0;
}
