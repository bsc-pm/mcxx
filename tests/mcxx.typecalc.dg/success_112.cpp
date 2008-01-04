template <typename _Ret,
         typename _Param1>
void functor(_Ret (*)(_Param1));

template <typename _T, typename _Q>
_T f(_Q);

void g()
{
    functor(f<int, float>);
}
