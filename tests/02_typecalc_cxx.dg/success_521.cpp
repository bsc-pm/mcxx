/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A {
    A();
    operator int();
    operator double();
};

A a;

int i = a;
