struct B { };

struct A { };

A f();

void g()
{
	A a;

	a = f();
}
