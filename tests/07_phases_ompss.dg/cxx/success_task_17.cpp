/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>
*/

#include <cstdlib>
#include <map>

int main (int argc, char *argv[])
{
	std::map<int, int> m;

    for (int i = 0; i < 100; i++)
    {
        m[i] = i;
    }

	for (std::map<int, int>::iterator it = m.begin();
            it != m.end();
            it++)
	{
		#pragma omp task
		{
            it->second++;
		}
	}

#pragma omp taskwait

    for (int i = 0; i < 100; i++)
    {
        if (m[i] != (i + 1))
            abort();
    }

    return 0;
}
