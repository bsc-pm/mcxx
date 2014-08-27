/*
<testinfo>
test_generator=config/mercurium
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

struct  B : A
{
    typedef A base;

    public:
    typedef A::bool_type bool_type;
    operator bool_type()
    {
      return base::operator bool_type();
    }
};
