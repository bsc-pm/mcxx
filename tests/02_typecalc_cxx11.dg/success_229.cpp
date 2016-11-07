/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/
enum class KK { KK1, KK2 };

void foo(KK var)
{
    switch (var)
    {
        case KK::KK1:
        {
            break;
        }
        case KK::KK2:
        {
            break;
        }
        default:
        {
            break;
        }
    }
}

void foo2()
{
    switch (KK var = KK::KK1)
    {
        case KK::KK1:
        {
            break;
        }
        case KK::KK2:
        {
            break;
        }
        default:
        {
            break;
        }
    }
}
