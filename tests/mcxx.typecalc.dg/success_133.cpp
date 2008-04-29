template <typename _T>
void f(_T* t)
{
    t->_T::~_T();
}

struct A
{
};

void g()
{
    typedef int Q;
    Q *p;
    f(p);

    p->Q::~Q();

    A *a;
    f(a);

    a->A::~A();
}
