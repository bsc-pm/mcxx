/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A {};

A g_a;

struct B
{
  A &a_;
  int x;
  B(int) : a_(g_a) { }
  B(const B& b) : a_(b.a_) { }
};

void f()
{
    const B &b = B(1);

    b.a_ = A();
}
