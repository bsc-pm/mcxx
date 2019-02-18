/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
class A
{
    public:
        virtual ~A() { }
};

class B : A
{
    public:
        ~B() override;
};
