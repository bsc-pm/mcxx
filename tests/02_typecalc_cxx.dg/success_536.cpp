/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    A();
    ~A();
};

void f(A<int> *a)
{
    a->A<int>::~A<int>();
    a->A<int>::~A();
    a->A::~A();
    a->A::~A<int>();
}

template <typename T>
void g(A<T> *a)
{
    a->A<T>::~A<T>();
    a->A<T>::~A();
}
