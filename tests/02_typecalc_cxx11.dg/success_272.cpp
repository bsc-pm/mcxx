/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 9)
#include<initializer_list>
#define TEST_INITIALIZER_LIST
#endif

struct A {
  A();
  A(int);
};

struct B 
{
    B(int, float, char);
};

#ifdef TEST_INITIALIZER_LIST
struct C
{
    C(std::initializer_list<int>);
};
#endif

void foo(A a = {});
void foo2(A a = {42});

void foo3(B b = {1, 2.3f, 'a'});

#ifdef TEST_INITIALIZER_LIST
void foo4(C c = {1, 2, 3, 4, 5});
#endif

void bar() {
  foo();
  foo2();
  foo3();
#ifdef TEST_INITIALIZER_LIST
  foo4();
#endif
}

