/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A{
   A(){} 
   A(int a){}
};

template <typename T>
struct B
{
  static A foo;
};

template<> A B<int>::foo;
int bar = 0;
template<> A B<int>::foo( bar );
