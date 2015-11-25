/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

inline bool endianness() {
  const int x = 1;
  return ((unsigned char*)&x)[0]?false:true;
}
