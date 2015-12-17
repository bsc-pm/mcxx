/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

static_assert(1 < 2, "");
static_assert(1 <= 2, "");
static_assert(2 >= 1, "");
static_assert(2 > 1, "");

static_assert(1 == 1, "");
static_assert(1 >= 1, "");
static_assert(1 <= 1, "");

static_assert(1 != 2, "");
