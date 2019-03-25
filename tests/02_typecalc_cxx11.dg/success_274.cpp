/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A {
  void bar();
};

A foo();

struct B : decltype(foo()) {};

void inherits_from_A(A& a);
void test1()
{
    B b;
    b.bar();
    inherits_from_A(b);
}

template <typename T> struct C : decltype(T()) {};

void quux() {
  C<A> c;
  c.bar();
  inherits_from_A(c);
}
