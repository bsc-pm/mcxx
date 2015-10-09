/*
<testinfo>
test_generator="config/mercurium-omp c++11"
</testinfo>
*/
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
