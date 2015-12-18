/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

constexpr int select(bool p, int a, int b){
    return p ? a : b;
}

static_assert(select(true, 1, 2) == 1, "");
static_assert(select(false, 1, 2) == 2, "");
