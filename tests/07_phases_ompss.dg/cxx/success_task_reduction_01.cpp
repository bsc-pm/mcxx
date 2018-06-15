/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
</testinfo>
*/

namespace N
{
    struct A
    {
        void foo();
    };

    void A::foo()
    {
        int x = 0;

        #pragma omp task reduction(+: x)
        {
            x++;
        }

        #pragma omp taskwait
    }
}

int main()
{
    N::A a;
    a.foo();
}
