/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{
    template<class T>
        void* apply(void* address) const
        {
            char * next;
            next = address = this->template apply<T>(address);
        }
};
