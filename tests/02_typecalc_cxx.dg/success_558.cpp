/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct Token
{
  operator long() const;
  operator int() const;
};

void foo()
{
    {
        Token t;
        int p;
        p = t;
    }

    {
        Token *t;
        int *p;
        *p = *t;
    }
}
