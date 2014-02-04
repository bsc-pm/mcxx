/*
<testinfo>
test_generator="config/mercurium-cxx11 run"
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
    // printf("this is a %sC++11 compiler, version is %ld\n", (cpp98 ? "pre-" : ""), __cplusplus);
    assert((__cplusplus == 199711 && cpp98)
            || (__cplusplus == 201103 && !cpp98));
    return 0;
}
