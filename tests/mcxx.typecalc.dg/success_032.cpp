template <typename _T>
_T f(_T*);

void h(int (*)(int*));

void g()
{
    int (*k)(int*);

    k = f;
    h(f);
}
