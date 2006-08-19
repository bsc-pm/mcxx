struct B { };

template <class T = int B::* (float*, B&) >
struct A
{
};
