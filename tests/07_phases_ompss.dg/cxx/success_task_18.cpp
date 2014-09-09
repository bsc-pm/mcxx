/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>
*/
template < typename T>
struct C
{

    void foo()
    {
        #pragma omp task inout(p)
        {
            p = 0;
        }
    }

    T *p;
};


int main()
{
    C<int> c;
    c.foo();

#pragma omp taskwait
}
