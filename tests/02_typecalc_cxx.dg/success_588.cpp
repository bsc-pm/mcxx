/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct Visitor
{
};

typedef void (*VisitorCallback)(Visitor*, void* self);
typedef VisitorCallback TraceCallback;

template<typename T, void (T::*method)(Visitor*)>
struct TraceMethodDelegate {
    static void trampoline(Visitor* visitor, void* self) { (reinterpret_cast<T*>(self)->*method)(visitor); }
};

class PersistentNode {
    public:
        explicit PersistentNode(TraceCallback trace);
};

class PersistentAnchor : public PersistentNode {
    public:
        void trace(Visitor* visitor);

    private:
        PersistentAnchor() : PersistentNode(TraceMethodDelegate<PersistentAnchor, &PersistentAnchor::trace>::trampoline)
    {
    }
};

