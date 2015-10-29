/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct A
{
    explicit A(int x);
    bool end() const;
    void next();
};

void f(int k)
{
    for (A a(k); !a.end(); a.next())
    {
    }
    for (A a(k), b(k+1); !a.end() && !b.end(); a.next(), b.next())
    {
    }
}
