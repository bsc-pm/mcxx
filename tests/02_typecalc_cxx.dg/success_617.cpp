/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template < typename T>
class A
{
    T x; // This forces 'T' to be  a completed type
};


class B
{
    // This should force the instantiation of 'A<B>'
    B(A<B> x) {}
};


