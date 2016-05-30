/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/

namespace {
    void foo(int i, float f) {
    }
}

void foo(float f) {
    ::foo(1, 3.0f);
}
