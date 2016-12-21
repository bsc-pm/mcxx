/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/
struct Data
{
    void foo() {}
};

struct A
{
    Data d;
    Data operator->() { return d; }
};

struct C
{
    A _a;
    A& operator->() { return _a; }
};

int main() {
    C c;

    // The last implicit call to the arrow operator has to have pointer to class type
    c->foo();
}
