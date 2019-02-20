/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
struct A
{
    virtual void foo() {}
};

struct B : public A
{
    void foo() override {}

};

int main()
{
    B b;
    auto f = [&]() {
        b.foo();
    };

    f();
}
