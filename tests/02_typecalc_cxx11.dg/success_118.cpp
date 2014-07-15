/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
enum struct E
{
    V, V2
};

void m(...);

void f()
{
    E e1;
    e1 = E::V;
    E e2;
    e2 = E::V;

    E::V == E::V2;
    E::V < E::V2;
    E::V <= E::V2;
    E::V > E::V2;
    E::V >= E::V2;

    m(E::V);
}
