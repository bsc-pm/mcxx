/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A { };
struct B
{
  operator A();
};

B& f();

void foo(void)
{
   ( A(f()) );
}

