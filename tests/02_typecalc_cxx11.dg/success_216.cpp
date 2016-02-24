/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

typedef decltype(nullptr) nullptr_t;

constexpr int bar(nullptr_t n) { return 1; }
constexpr int foo(nullptr_t n) { return bar(n); }

void g()
{
    foo(nullptr);
    foo(0);
    foo(__null);
}
