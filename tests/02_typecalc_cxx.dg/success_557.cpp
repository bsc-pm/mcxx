/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct SLIType{};
 struct Dictionary{};
 struct SLIInterpreter
 {
   static SLIType Dictionarytype;
 };

 template <class D, SLIType *slt>
 class lockPTRDatum {};

 typedef lockPTRDatum<Dictionary,&SLIInterpreter::Dictionarytype>
 DictionaryDatum;

 template<typename T>
 void append_property(DictionaryDatum &d, const T &prop)
 {}

 template<>
 void append_property<long> (DictionaryDatum &d, const long &prop)
 {}
