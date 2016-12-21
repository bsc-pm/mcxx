/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct Data
{
    void foo() {}
};

struct A
{
    Data d;
    Data* operator->() { return &d; }
};

struct C
{
    A _a;
    A& operator->() { return _a; }
};

int main() {
    C c;
    c->foo();
}
