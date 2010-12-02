/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes

test_compile_fail_nanox_instrument=yes
test_compile_faulty_nanox_instrument=yes
</testinfo>
*/

#include <vector>

int bar( int * a, const int * b)
{
    *a = *b + 1;
    return *a;
}

#pragma omp declare reduction( foo : int : bar (&_out,&_in) )

int main (int argc, char* argv[])
{
   std::vector<int> v(10,1);
   int x;

   #pragma omp parallel for reduction (foo : x)
   for (int i=0; i<10; i++)
   {
       x += v[i];
   }

   return 0;
}
