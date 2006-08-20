struct A { A(); };
typedef A global_A;
struct B { };
struct C: public A, public B { C(); };

C::C(): 
    global_A() 
{ 
} // mem-initializer for base A
