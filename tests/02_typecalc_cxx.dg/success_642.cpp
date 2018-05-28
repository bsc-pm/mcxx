/*
<testinfo>
test_generator="config/mercurium"
test_nolink=yes
</testinfo>
*/
namespace N
{
    namespace Data
    {
        enum E
        {
            default_value = -1,
        };

        namespace interface1
        {
            struct  A
            {
                E level;
                A( E e = default_value ) : level(e) {}
            };
        }

        using interface1::A;
    }
}

namespace N
{
    namespace Data
    {
        namespace interface1
        {
            class  B : public Data::A
            {
                public:
                    B( E e ) : Data::A(e) {}
            };
        }
    }
}
