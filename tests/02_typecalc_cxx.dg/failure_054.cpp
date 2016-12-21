/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/
struct D
{
    D operator->() {
        return *this;
    }

    void foo() {}
};

int main() {
    D d;

    d->foo();
}


