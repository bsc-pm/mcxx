/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace {
    void foo(int i, float f) { }
    void foo(float f) { foo(2, f); }
}
void foo(char c) {
    foo(1, 0.0f);
}
