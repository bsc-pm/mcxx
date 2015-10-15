/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

typedef short unsigned int UChar;

template<typename T, UChar (...Converter)(T)>
static unsigned computeHashAndMaskTop8Bits(const T* data, unsigned length);

class CaseFoldingHash
{
    public:
        template<typename T> static UChar foldCase(T ch);

        static unsigned hash(const UChar* data, unsigned length)
        {
            return computeHashAndMaskTop8Bits<UChar, foldCase<UChar> >(data, length);
        }
};

void f(UChar* c, unsigned l)
{
    CaseFoldingHash::hash(c, l);
}
