/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes

test_compile_fail_nanox_instrument=yes
test_compile_faulty_nanox_instrument=yes
</testinfo>
*/

#include <algorithm>
#include <functional>
#include <vector>

#pragma omp declare reduction( + : std::vector<int> : \
std::transform(_in.begin( ), _in.end( ), \
_out.begin( ), _out.begin ( ), std::plus<int >() ) )

#pragma omp declare reduction( merge: std::vector<int>: \
_out.insert(_out.end(), _in.begin(), _in.end() ) )

int main (int argc, char* argv[])
{
   std::vector<int> v1(5);
   std::vector<int> v2(5);

   #pragma omp parallel for reduction(merge : v2)
   for (int i=0; i<5; i++)
   {
      v1 = v2;
   }

   return 0;
}
