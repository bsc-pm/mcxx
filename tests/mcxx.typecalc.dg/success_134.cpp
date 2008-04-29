template <typename _T>
struct A
{
};

template <typename _Q>
void f(A<_Q> q);

struct C : virtual A<int> { };

void g(void)
{
    C c;
    f(c);
}
