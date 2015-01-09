/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct Base
{
    Base(int*, unsigned int);
};

struct A : Base
{
    A();
    A(int *x, unsigned int y);
    A operator+(int) const;
};

A foo(int x, const A& b)
{
    return b + x;
}
