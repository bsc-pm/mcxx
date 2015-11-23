/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
typedef unsigned long long foo_uint64_t;
typedef unsigned int foo_uint32_t;

struct foo_entry_s {
 unsigned int _1:2;
 unsigned int _2:2;
 unsigned int _3:3;
 unsigned int _4:4;
 unsigned int _5:1;
 unsigned int _6:20;
 unsigned int _7:9;
 unsigned int _8:2;
 unsigned int _9:1;
 unsigned int _10:20;
};

typedef union foo_entry_e foo_entry_t;
union foo_entry_e {
 foo_uint64_t _dword;
 union {
  struct foo_entry_s _fields;
  struct {
   foo_uint32_t low;
   foo_uint32_t high;
  };
 };
};

 foo_entry_t tmp_entry1 = { ._fields = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, } };

void f()
{
 foo_entry_t tmp_entry2 = { ._fields = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, } };
}
