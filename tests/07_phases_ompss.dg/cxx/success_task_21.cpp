/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>
*/
#include <stdlib.h>

template <typename T>
struct A
{
    T t;
    A() : t(0) { }
};

template <typename T>
void f(A<T> *a, int first, int last, int val)
{
#pragma omp task out(a[first:last])
    {
        int i;
        for (i = first; i <= last; i++)
        {
            a[i].t = val;
        }
    }
}

template <typename T>
void g(A<T> *a, int length, int val)
{
#pragma omp task in(a[0;length])
    {
        int i;
        for (i = 0; i < length; i++)
        {
            if (a[i].t != val)
            {
                abort();
            }
        }
    }
}

int main(int argc, char *argv[])
{
    A<int> a[10];

    for (int j = 0; j < 1000; j++)
    {
        f(a, 0, 9, j);
        g(a, 10, j);
    }

#pragma omp taskwait
}
