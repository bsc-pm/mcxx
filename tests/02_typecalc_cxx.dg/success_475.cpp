/*
<testinfo>
test_generator="config/mercurium run"
</testinfo>
*/

#include <stdlib.h>

namespace
{
   struct A
   {
      static int x;
   };

   struct B
   {
      static int x;
   };

   struct C
   {
       struct D
       {
           static int x;
       };
   };

   int A::x = 3;
   int B::x = 4;
   int C::D::x = 5;
}

int main(int argc, char *argv[])
{
    if (A::x != 3) abort();
    if (B::x != 4) abort();
    if (C::D::x != 5) abort();

    return 0;
}
