struct A
{
    int a;

    template <typename _T>
    operator _T()
    {
        return this->a;
    }
};
