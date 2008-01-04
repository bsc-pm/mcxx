template <typename _T1,
         _T1 (*_PF1)(_T1),
         _T1 (*_PF2)(_T1)>
struct B
{
};

template <typename _T2,
         _T2 (*_PF3)(_T2)>
struct B<_T2, _PF3, _PF3>
{
    typedef _T2 T;
};

float f(float);
int f(int);

B<int, f, f>::T t;

