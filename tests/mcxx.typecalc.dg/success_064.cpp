template <typename _T, typename _Q>
void f(_T*, _Q*, _T, _Q);

class A
{
        friend void f<>(int*, char*, int, char);
};
