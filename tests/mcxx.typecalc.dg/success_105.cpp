template <int _I>
void f(int (*a)[_I]);

void h()
{
    f<10>(0);
}
