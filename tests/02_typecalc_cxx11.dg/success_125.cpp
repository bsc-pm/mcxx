/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

typedef decltype(nullptr) nullptr_t;

struct A
{
    explicit A(void*);
    A();
    A(nullptr_t);
};

bool operator==(const A&, const A&);

void f()
{
    A a;
    a == 0;
}
