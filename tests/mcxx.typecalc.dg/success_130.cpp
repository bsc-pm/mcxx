
struct B { };

struct A { };

A f();

void g(A);

void g()
{
	A a;
	g(f());
}
