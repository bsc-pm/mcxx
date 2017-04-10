/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template < typename T >
class basic_istream
{
};

template < typename T>
class basic_istringstream : public basic_istream<T>
{
};

void foo(basic_istringstream<char>& var)
{
    static_cast< basic_istream<char>&>(var);
}
