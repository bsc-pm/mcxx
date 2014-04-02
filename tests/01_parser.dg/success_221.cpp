/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace B
{
    namespace D
    {
        namespace I
        {
            template <typename S>
                struct tpl;
        }
    }
}

namespace B
{
    namespace P
    {
        namespace I = B::D::I;

        namespace D
        {
            template <typename T>
            void f()
            {
                I::tpl<T>::value = 3;
            }
        }
    }
}
