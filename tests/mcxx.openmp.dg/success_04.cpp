template <typename _T>
void f(_T t)
{
#pragma omp parallel
    {
        t = 3;
    }
}
