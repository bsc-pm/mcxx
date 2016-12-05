/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template < typename T >
class C
{};

class A
{
    template < class T1 >
    operator C<T1>();

    template < class T1 >
    T1 foo();
};

template < typename T2 >
A::operator C<T2>()
{}

template < class T2 >
T2 A::foo()
{}
