/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

typedef unsigned long uint64_t;

namespace err
{
    extern const char *fnv1_runtime_error;
}

namespace detail
{
  constexpr uint64_t fnv1(uint64_t h, const char* s)
  {
    return (*s == 0) ? h :
      fnv1((h * 1099511628211ull) ^
           static_cast<uint64_t>(*s), s+1);
  }
}
constexpr uint64_t fnv1(const char* s)
{
  return true ? detail::fnv1(14695981039346656037ull, s) :
      throw err::fnv1_runtime_error;
}

static_assert(fnv1("abc") == 15626587013303479755ull, "");
