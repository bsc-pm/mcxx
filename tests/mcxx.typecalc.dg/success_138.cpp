template <typename _Class, typename _Member>
double * f(_Member (_Class::* p)());

template <typename _Class, typename _Member>
int * f(_Member _Class::* p);

struct A
{
    int d_a;

    int d_f();
};

void h1(int*);
void h2(double*);

void g()
{
    f(&A::d_a);
    f(&A::d_f);
}
