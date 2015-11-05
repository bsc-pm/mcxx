/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

#include <cstdint>

const int32_t a = -2;

const int32_t b = ~a;


static_assert(b == 1, "");
