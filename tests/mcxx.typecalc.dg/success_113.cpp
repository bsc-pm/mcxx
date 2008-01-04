template <typename _Ret,
         typename _Class,
         typename _Param1>
void functor(_Ret (_Class::*)(_Param1));

struct A
{
    float f(int);
    static float g(int);
};

void g()
{
    functor(&A::f);
}
