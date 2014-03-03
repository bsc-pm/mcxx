/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A
{
    A(int x = 0);
};

struct B
{
    union
    {
        // this union does not have a deleted default constructor!
        A a;
    };

    B() { }
};

B b;
