/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
template < typename T>
class List { };

enum M { VAL1, VAL2 };

typedef int K;
template < int S = 0 >
List< K > foo() {}

template <typename T>
class X
{
    template <  M val = VAL1 >
    List< T > foo2() {}
};
