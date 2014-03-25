/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{
    typedef void (A::* foo)(int);

    void bar(int);

    foo g(bool b )
    {
        return b ? &A::bar : 0;
    }
};
