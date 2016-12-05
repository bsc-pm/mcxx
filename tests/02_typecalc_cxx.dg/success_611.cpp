/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
class File;

class String
{
    public:
     String(const String& f);
     String(const File& f);
};

class File
{
    public:
    operator String() const;
    void f() const
    {
        String a2(*this);
    }
};
