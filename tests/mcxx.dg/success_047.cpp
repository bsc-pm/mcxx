struct A
{
	void f(const int a);
	void g(const int* a);
};

void A::f(int a)
{
}

void A::g(const int* const a)
{
}
