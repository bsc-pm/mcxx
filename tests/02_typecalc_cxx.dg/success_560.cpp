/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{
   A(int a);
};

struct C
{
   C();
};

template <typename T>
struct B
{
  static A foo;
  static C quux;
};

template<>
A B<int>::foo; // This is valid, surprisingly
int bar = 0;
template<>
A B<int>::foo( bar );

template<>
C B<int>::quux;
