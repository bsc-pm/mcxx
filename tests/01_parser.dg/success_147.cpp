/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace detail { struct none_helper{}; }

typedef int detail::none_helper::*none_t;

void bar(none_t n);

void foo(none_t n)
{
  bar(n);
}
