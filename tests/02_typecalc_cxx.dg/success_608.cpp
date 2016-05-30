/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace  {
    namespace {
        void foo(int i, float f) {
        }
    }

    void foo(float f) {
        foo(1, 3.0f);
    }
}

void foo(float f) {
    foo(1, 3.0f);
}
