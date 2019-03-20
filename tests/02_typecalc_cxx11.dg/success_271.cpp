/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A
{
    virtual ~A();
};

struct B : A
{
    virtual ~B() override { }
};

