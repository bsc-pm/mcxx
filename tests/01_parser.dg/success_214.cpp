/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

typedef void (*PF)();

class List
{
    PF* items_;
    int size_;
    List() {
        items_ = new PF;
        items_ = new PF();
        items_ = new PF[size_];
    }
};
