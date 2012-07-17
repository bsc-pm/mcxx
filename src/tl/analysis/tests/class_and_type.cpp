#include <cstddef>
#include <iostream>
#include <string>
#include <typeinfo>

class MyClass
{
private:
    int _id;
    std::string _name;
    
public:
    MyClass(int id, std::string name) : _id(id), _name(name) {}
    int get_id() const { return _id; }
    void set_name(std::string s) { _name = s; }
};

struct S {
    int m0;
    char m1;
    short m2;
    long m3;
};

typedef int myInt;

int foo(int argc, char** argv)
{
    // *** Class methods *** //
    MyClass a(5, "myclass");
    
    int id = a.get_id();
    int id2 = (&a)->get_id();
    
    if (id != id2)
        return 1;
    
    // *** Type methods *** //
    int size = sizeof(a);
    int align = __alignof__(int);
    int offset_of = offsetof(S, m2);
    long *offset = (long *)(&((struct S *)0)->m3);
    
    a.set_name(std::string(typeid(a).name()) + "_a");
    
    myInt i = 5;
    
    return 0;
}