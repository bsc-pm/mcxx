template <typename _Ret,
         typename _Param1>
void functor(_Ret (*)(_Param1));

struct A
{
    float f(int);
    static float g(int);
};

void g()
{
    functor(A::g);
}
