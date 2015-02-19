/*
<testinfo>
test_generator=config/mercurium
test_CXXFLAGS="-Werror"
</testinfo>
*/
struct TokenArrayObj {};

struct TokenArray
{
    TokenArrayObj* data;
    operator TokenArrayObj() const { return *data; }
    TokenArray(const TokenArray &a) : data(0) {}
};

struct TokenStack : private TokenArrayObj
{
    TokenStack(const TokenArray& ta) : TokenArrayObj(ta) {}
};
