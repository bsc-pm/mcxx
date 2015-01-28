/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template<typename T>
struct A;

template<typename Ret, typename ...T>
struct A<Ret(T...)>
{
    A(Ret (*)(T...));
};

int main(int argc, char** argv)
{
    A<int(float x)> foo ((
        [&](float x) -> int
        {
            return x + 1;
        }
        ));
}
