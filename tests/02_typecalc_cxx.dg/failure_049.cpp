/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/

void foo(char c) {
    foo(1, 0.0f);
}

namespace {
    void foo(int i, float f) { }
    void foo(float f) { ::foo(2, f); }
}
