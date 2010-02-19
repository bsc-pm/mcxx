/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
template <typename _Ret, typename _Par1>
struct Functor { };

template <typename _Ret, typename _Par1>
Functor<_Ret, _Par1> functor(_Ret (*)(_Par1));

template <typename _Ret, typename _Class>
Functor<_Ret, _Class> functor(_Ret (_Class::*)());

struct A
{
    int foo();
};

struct B : A
{

};

void bar()
{
    functor(&A::foo);
    functor(&B::foo);
    functor<int, B>(&A::foo);
    functor<int, A>(&B::foo);
}
