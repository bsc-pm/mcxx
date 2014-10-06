/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{
   enum { FOO };
   static int x;
};

int A::x(FOO);
