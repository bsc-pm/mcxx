template <class T, class U = double>
void f1(T t = 0, U u = 0);

template <class T, class U = T*>
void f2(T t = 0, U u = 0);

void g()
{
    f1<int>();
    f2<int>();
}
