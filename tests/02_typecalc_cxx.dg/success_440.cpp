/*
<testinfo>
test_generator="config/mercurium run"
</testinfo>
*/

#include <assert.h>

bool const one = true;
int const two = 2;
int const three = 3;

template< int >
struct fun {
  typedef int two;
};

template< typename T >
struct fon
{
  static int const three = ::three;
  static bool const one = ::one;
};

bool const cpp98 = fon< fun< 1 >>::three >::two >::one;
//bool const as_cpp98 = fon< fun< (1 >>::three) >::two >::one; // valid for both
//bool const as_cpp11 = (fon< fun< 1 >>::three) >::two >::one; // not valid in C++98

int main(void)
{
    // Old versions of g++ incorrectly defined __cplusplus as 1
#if defined(__GNUC__)
  #if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 7)
      const long cxx_98_value = 199711L;
  #else
      const long cxx_98_value = 1;
  #endif
#else
    const long cxx_98_value = 199711L;
#endif


    assert((__cplusplus == cxx_98_value && cpp98)
            || (__cplusplus == 201103 && !cpp98));
    return 0;
}
