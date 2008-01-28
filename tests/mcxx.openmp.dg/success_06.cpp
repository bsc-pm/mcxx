template <typename _T>
struct A
{
    void f(_T t)
    {
#pragma omp parallel
        {
            t = 3;
        }
    }
};

