template <typename _T>
struct A
{
    struct B
    {
        void g();
        void f()
        {
            this->g();
        }
    };
};
