/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct B { };

struct P
{
    P(B*);
};

template < unsigned int N >
struct A : B
{
    double *pProperties;
    typedef short S;

    struct Something { };
    struct SomethingElse { Something bar(float*); };
    SomethingElse foo();

    A();
    A(S, Something, double*);

    P fails(S t, float *ThisNodes)
    {
        return P(new A<N>(t, this->foo().bar(ThisNodes), pProperties));
    }

    P good(S t, float *ThisNodes)
    {
        return P(new A(t, this->foo().bar(ThisNodes), pProperties));
    }

};

void g()
{
    A<100> a;

    a.fails(0, 0);
    a.good(0, 0);
}
