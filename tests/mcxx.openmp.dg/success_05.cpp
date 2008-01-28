template <typename _T>
struct A
{
    void f(_T t);
};

template <typename _Q>
void f(_Q q)
{
#pragma omp parallel
    {
        q = 3;
    }
}
