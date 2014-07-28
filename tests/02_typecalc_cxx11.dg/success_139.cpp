/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
};

int * begin(A&);
int * end(A&);

struct B
{
    int* begin();
    int* end();
};

void f()
{
    int begin, end;
    A a;
    for (int it : a)
    {
    }

    B b;
    for (int it : b)
    {
    }
}
