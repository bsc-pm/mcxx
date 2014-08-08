/*
<testinfo>
test_generator=config/mercurium
# We stumbled against GCC PR62064, this flag is a workaround
test_CXXFLAGS="-fno-access-control"
</testinfo>
*/
struct A
{
    typedef void (A::*bool_type)();
    operator bool_type()
    {
        return 0;
    }
};

struct  B : private A
{
    typedef A base;

    public:
    typedef A::bool_type bool_type;
    operator bool_type()
    {
      return base::operator bool_type();
    }
};
