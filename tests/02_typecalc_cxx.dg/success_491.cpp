/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
   T x;
   void f(T *);
   void h(T A::*);
   void g();
};

template <typename T>
void A<T>::g()
{
   f(&x);
   h(&A::x);
   // Emitting f(&A::x) is wrong but f(&(A::x)) is fine...
}

void m()
{
    A<int> a;
    a.g();
}
