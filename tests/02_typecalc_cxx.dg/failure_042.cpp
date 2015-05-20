/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/

namespace std
{
  typedef long unsigned int size_t;
  typedef long int ptrdiff_t;
  typedef ptrdiff_t streamsize;
}

namespace std __attribute__ ((__visibility__ ("default")))
{
    class ios_base
    {
        public:
            streamsize precision(streamsize __prec)
            {
                return 1;
            }
    };

    template<typename _CharT>
        class basic_ios : public ios_base
    {
    };
    template<typename _CharT>
        class basic_ostream : public basic_ios<_CharT>
    {
    };


    template<typename _CharT>
        class basic_istream : public basic_ios<_CharT>
    {
    };


    template<typename _CharT>
        class basic_iostream
        : public basic_istream<_CharT>,
        public basic_ostream<_CharT>
    {
    };
}
namespace std __attribute__ ((__visibility__ ("default")))
{
  template<typename _CharT >
    class basic_fstream : public basic_iostream<_CharT>
    {
    };
}

namespace std __attribute__ ((__visibility__ ("default")))
{
  typedef basic_fstream<char> fstream;
}


void foo(std::fstream& t)
{
    t.precision(8);
}
