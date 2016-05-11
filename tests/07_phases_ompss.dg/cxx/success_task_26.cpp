/*
<testinfo>
test_generator=config/mercurium-ompss
test_CXXFLAGS="--no-copy-deps"
</testinfo>
*/

#include <vector>
#include <algorithm>
#include <iostream>
#include <cstdlib>

template <typename T>
void set(std::vector<T *>& w)
{
    T **pw = w.data();
#pragma omp task out({ *(pw[i]), i = 0 : w.size() - 1 }) shared(w)
    {
        int k = 0;
        for (typename std::vector<T *>::iterator it = w.begin(); it != w.end();
             it++, k++)
        {
            T *n = *it;
            *n = k;
        }
    }
}

template <typename T>
void check(std::vector<T *>& w)
{
    T **pw = w.data();
#pragma omp task out({ *(pw[i]), i = 0 : w.size() - 1 }) shared(w)
    {
        int k = 0;
        for (typename std::vector<T *>::iterator it = w.begin(); it != w.end();
             it++, k++)
        {
            T *n = *it;
            T t = (*n - k);

            t = t < 0 ? -t : t;
            if (t > 1e-5)
            {
                std::cerr << "Value is out of valid bounds t=" << t
                          << " k=" << k << std::endl;
                abort();
            }
        }
    }
}

template <typename T>
struct GeneratePtr
{
    T *operator()()
    {
        return new T;
    }
};

int main(int argc, char *argv[])
{
    std::vector<float *> fw(1000, (float *)0);
    std::generate(fw.begin(), fw.end(), GeneratePtr<float>());

    std::vector<int *> iw(1000, (int *)0);
    std::generate(iw.begin(), iw.end(), GeneratePtr<int>());

    set(fw);
    set(iw);

    check(fw);
    check(iw);

#pragma omp taskwait

    return 0;
}
