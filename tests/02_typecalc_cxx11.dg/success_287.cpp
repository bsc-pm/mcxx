/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
struct A
{
    A() {}
    int foo() const {}
};

int bar() {
    const  A&& a1((A()));
    const  A&& a2 = A();

    return a1.foo() + a2.foo();
}
