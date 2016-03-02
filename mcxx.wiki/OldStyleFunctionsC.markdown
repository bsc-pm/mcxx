
 Old style functions are those functions declarations which do not provide a prototype, this is, a parameter list
but instead rely on an identifier-list (for function definitions only) or an empty list.

From the previous paragraph, one can deduce the following examples


        void f(a, b);


        void f();

Note that `void f();` in C (*but not in C++*) is entirely different to `void f(void);`.


        void f(a, b)
          int a;
          int b;
        {
         ...
        }

## Function compatibility

Excerpt from the C99 standard

 For two function types to be compatible, both shall specify compatible return types (if both function types are "old style", parameter types are not compared). Moreover, the parameter type lists, if both are present, shall agree in the number of parameters and in use of the ellipsis terminator; corresponding parameters shall have compatible types. *If one type has a parameter type list and the other type is specified by a function declarator that is not part of a function definition and that contains an empty identifier list, the parameter list shall not have an ellipsis terminator and the type of each parameter shall be compatible with the type that results from the application of the default argument promotions. If one type has a parameter type list and the other type is specified by a function definition that contains a (possibly empty) identifier list, both shall agree in the number of parameters, and the type of each prototype parameter shall be compatible with the type that results from the application of the _default argument promotions_ to the type of the corresponding identifier*. (In the determination of type compatibility and of a composite type, each parameter declared with function or array type is taken as having the adjusted type and each parameter declared with qualified type
 is taken as having the unqualified version of its declared type.)

## Function calls to old-style declared/defined functions

Excerpt from the C99 Standard

 If the expression that denotes the called function has a type that does not include a
 prototype, the _integer promotions_ are performed on each argument, and arguments that
 have type `float` are promoted to `double`. These are called the ''default argument 
 promotions''.
 
 If an `int` can represent all values of the original type, the value is converted to an `int`;
 otherwise, it is converted to an unsigned `int`. These are called the _integer promotions_

## GCC extension on this topic

According to [gcc documentation](http://gcc.gnu.org/onlinedocs/gcc/Function-Prototypes.html), gcc adds prototype information to function definitions (since they will have full parameter declaration).

This means that


        #!cpp
        void f(int, float);
        void f(a, b)
          int a;
          float b;
        {
         ...
        }

is equivalent to


        #!cpp
        void f(int, float);
        void f(int a, float b)
        {
         ...
        }

which seems a sensible approach since nowadays old-style functions are more of a hassle than anything useful. Notwithstanding this extension (which makes old-style functions much more bearable) *they are an obsolescent feature of C99 and should not be used in any new development under any circumstance*.

[[FootNote]]