
 Branch `experimental_garbage_collection` integrates the Boehm-Demers-Weiser garbage collector. It is a conservative garbage collector (it is not precise). It works by scanning the heap and the stack looking for words that look like pointers. It is used in many environments (like [gcj](http://gcc.gnu.org/java/), [Mono](http://www.mono-project.com/Mono:Runtime), [Ruby](http://edwinmeyer.com/Release_Integrated_RHG_09_10_2008/chapter05.html), etc.) so it has been heavily proved functional and useful.

## Software requirements

!Debian/Ubuntu: `apt-get install libgc-dev`

!Redhat/Fedora/SuSE: `yum install gc-devel`

## Changes in the C interface

The usual C interface of `malloc`, `calloc`, `realloc`, `free` and `strdup` *cannot be used anymore* (as it would allocate memory outside of the control of the GC). Use `gc_malloc`, `gc_calloc`, `gc_realloc` and `gc_strdup` instead. Their semantics are the same as their standard counterparts.

All the code in the C part must use


        #include "gc_memory.h"

An (accidental) attempt to use `malloc`, `calloc`, `realloc`, `free` or `strdup` will result in a compilation failure as these names have been _poisoned_ by macros.

By general rule, do not call `gc_free` (let the garbage collector do its work) unless freeing very big arrays.

`gc_malloc` clears the memory like `gc_calloc` does.

Big chunks of memory should be allocated using `gc_malloc_off_page` although typically you should not need these.

## Changes in the C++ interface

### STL containers

*Do not use any Standard C++ container* like `std::vector`, `std::map`, etc.

Instead include `tl-gc-type.hpp` and use `gc_types<T>::Container`. These types are exactly the same of the STL but they use the `gc_allocator` instead of the default C++ allocator.

Example:


        #include "tl-gc-type.hpp"
        
        void f()
        {
          // std::vector<int> a; /* WRONG */
          gc_type<int>::vector a; /* RIGHT */
          // std::vector<int, gc_allocator<int> > a; /* OK but cumbersome, do not use, use gc_type<int>::vector instead */
        }

There is no way to enforce this rule, so be careful.

### Dynamic memory

*Do not use new*. Instead include `tl-gc-new.hpp` and use `NEW`.

Example:


        #include "tl-gc-new.hpp"
        void f(void)
        {
          int *i;
          // i = new int(3); // WRONG
          i = NEW int(3);
          // i = new (UseGC) int(3); // OK, but cumbersome, use NEW instead
        }

There is no way to enforce this rule, so be careful.

### Delete

*Do not call delete*. Ever.

If you need to call the destructor, use the following syntax `p->Class::~Class()`. You should not need this. though.

There is no way to enforce this rule, so be careful. 

# References

[A garbage collector for C and C++](http://www.hpl.hp.com/personal/Hans_Boehm/gc/)