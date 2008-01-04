void f(const char * c);

void g()
{
    f(__func__);
    f(__FUNCTION__);
    f(__PRETTY_FUNCTION__);
}
