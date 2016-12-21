/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/
struct D;

struct C
{
    D* d;
    D& operator->() {
        return *d;
    }
    void foo() {}
};

struct D
{
    C* c;
    C& operator->() {
        return *c;
    }

    void foo() {}
};

int main() {
    D d;

    d->foo();
}


