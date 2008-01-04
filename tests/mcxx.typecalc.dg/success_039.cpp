template <typename _T>
int* f(...);

template <typename _T>
float* f(int _T::*);

void h1(int*);
void h2(float*);

struct A
{
};

void g()
{
    h1(f<int>(0));
    h2(f<A>(0));
}
