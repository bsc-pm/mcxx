/*
<testinfo>
test_generator="config/mercurium"
test_nolink=yes
</testinfo>
*/
class Base
{
    protected:
    int x;
};

template < typename T>
struct Derived : public Base
{
};

template < typename T>
struct S : public Derived<T>
{
    using Base::x;

    void foo()
    {
        x = 2;
    }
};

int main()
{
    S<int> s;
}
