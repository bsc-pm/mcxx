/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>
*/

namespace N
{
    struct A
    {
        void foo()
        {
            #pragma omp task
            bar();

        }
        void bar()
        {
            int var;
            #pragma omp task
            {
            }
            #pragma omp taskwait
        }
    };
}

int main()
{
    struct N::A a;

    a.foo();
    #pragma omp taskwait
}
