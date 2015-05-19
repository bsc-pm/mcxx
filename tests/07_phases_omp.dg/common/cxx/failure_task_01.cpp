/*
<testinfo>
test_generator=config/mercurium-omp
test_compile_fail=yes
</testinfo>
*/
struct A
{
    int m[4];
    void foo()
    {
        #pragma omp task firstprivate(m[2])
        {
        }
    }
};

int main()
{
   A a;
   a.foo();
#pragma omp taskwait
}
