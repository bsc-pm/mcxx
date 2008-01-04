template <typename _Ret,
         typename _Param1>
void functor(_Ret (*)(_Param1));

int f(float);

void g()
{
    functor(f);
}
