/*
<testinfo>
test_generator="config/mercurium-omp c++11"
</testinfo>
*/

// Intel 15.0.3 accepts this code but Intel 15.0.2 doesn't :(
#if !defined(__ICC) || (__ICC > 1500) || (__ICC == 1500 && __INTEL_COMPILER_UPDATE >= 3)

#include <vector>
#include <algorithm>
#include <cstdlib>
#include <cassert>

int main(int, char **)
{
    std::vector<int> v(200);
    std::generate(v.begin(), v.end(), std::rand);

    #pragma omp task shared(v)
    {
         std::sort(v.begin(), v.end(),
         [](const int &a,
            const int &b)
         -> bool { return a < b; }
         );
    }

#pragma omp taskwait

    for (std::vector<int>::iterator it = v.begin();
            it != v.end();
            it++)
    {
        if (it + 1 != v.end())
        {
            assert((*it) < *(it+1));
        }
    }

    return 0;
}
#else
int main(int argc, char *argv[])
{
    return 0;
}
#endif
