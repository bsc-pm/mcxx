template <typename _T>
struct A 
{ 
};

template <typename _T>
struct B : A<int>
{
};

template <typename _T>
void f(A<_T>*);

void g(void)
{
    B<float> *b;

    f(b);
}
