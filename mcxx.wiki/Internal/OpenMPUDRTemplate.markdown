# OpenMP User Defined Reductions and templates



        <p style="color: red; font-size: 16pt;">This information is outdated and kept here only for historical references. These proposals have been deprecated after OpenMP 4.0 declare reduction syntax.</p>


This page tries to describe how template user defined reductions work in C++. This should be as compiler-agnostic possible but maybe some details of the Mercurium implementation will be used to make the explanation clearer.

## Declaration, identification and use

### Declaration

A UDR is declared using 


        #pragma omp declare reduction (operator-name : type)

for convenience in C++ it is possible to specify a list of operators in `operator-name` and a list
of types in `type`. For the purpose of this explanation, those declarations are like declaring each
UDR at a time.

When declaring a UDR it should be checked, by the compiler

 * That the UDR is not being redeclared (we could relax this restriction allowing redeclarations but the compiler should find that it was redeclared anyway)
 * That the `operator-name` is an eligible function for the `type`. This means that the `operator-name` should match *one and only one* of the valid prototypes for UDR operators.

### Name of an UDR

What identifies a UDR is the tuple `<operator-name, type>`.

### Use

A UDR is used by means of the `reduction` clause of `parallel`, `for` or `sections` (or any 
`parallel`-compound version of the aforementioned). 


        #pragma omp ... reduction(operator-name : variable-list)


(Note that here `operator-name` is not a list).

The compiler should check that:

 * For each `variable` in the `variable-list` there is an eligible UDR of the form `<operator-name, typeof(variable)`.

## In C everything is very easy

Type system of C is rather simple compared to the one of C++ so OpenMP UDR come with the following restrictions

 * If the user declares two UDR with different types but the same `operator-name` and one is valid (so the `operator-name` is an eligible function whose prototype is valid for an UDR) then the other will be invalid because there is no way that the same `operator-name` can be eligible for two different types.
 * Because of this, when a UDR is used, the variables in `variable-list` will be all of the same type.

Don't forget that it is perfectly valid to declarate two UDR with the same type but different `operator-name`

# C++ without templates

C++ is so complex that we will split this discussion in two parts. The first part will not consider templated UDRs.

## Overload

C++ supports overload, this means that we can have several functions with several prototypes using the same name


        int foo(int, int);
        float foo(float, float);

It seems sensible to allow using overloaded names in UDR for a better integration in C++. This, however, breaks some
of the simple assumptions done so far.

 * Because of overloads `operator-name` is now a less stringent part of the key of the UDR. Even if the key of UDR is still the tuple `<operator-name, type>`, somehow `operator-name` is a weaker element of it.

What does this mean? It means that given an `operator-name` we still have to figure the precise function being named. So, in the following example


        #pragma omp declare reduction(foo : float)

we have to find that `foo` here means `foo(float, float)`. How can the compiler discover this?

The compiler can test all the eligible prototypes of a given type and see if *one and only one* of the overloaded names matches *at least one* of the valid prototypes. This operation is slow, because it may involve lots of tests, but it works. For the previous example the compiler did something like this

  1. Get all the prototypes that could be used for `float` type. This includes, among others, `float (float, float)` and `void (float&, float)` etc.
  2. For each prototype try to perform an overload resolution agains the overloaded 'foo' name. So, for `float (float, float)` we try to match the first argument `float` against the first parameter `float` and the second argument `float` against the second parameter `float`. Likewise, for `void (float&, float)` we will try to match the first argument `float&` against the first parameter `float` and the second argument `float` against the second parameter `float`.  (Note it may happen that more than one prototype checked against the overload set returns the same function, this is not an error, the error would be if two prototypes returned two different functions).
  3. A similar process (with only one parameter) is done with member functions.

The attentive reader will be wondering why are we relying on overload instead of a plain type comparison. Well, it turns that overload can cope with template functions, which is the next step.

## Template operators

Note that this does not mean that our UDR is templated, just that we are naming a templated operator.

Consider the case


        template <typename _T>
        void foo(_T&, const _T&);

we could declare a reduction like


        #pragma omp declare reduction(foo<int> : int)

which is the easy case since we are fully identifying the desired `foo<int>(int&, const int&)`. But the generic user will prefer the following form


        #pragma omp declare reduction(foo : int)

so the compiler has to deduce we meant `foo<int>`. Using overload this is pretty straightforward: one of the valid prototypes for an UDR on the type `int` will be of the form `void (int&, const int&)` and this one will match perfectly. Any other will fail miserably because of `const`-ness of the second argument and non-`const`ness of the first. 

For another example


        template <typename _T>
        _T bar(_T, _T);
        
        #pragma omp declare reduction(bar : float)

the compiler must deduce that we mean `bar<float>`. In this case overload would generate several cases (like `float& bar(float&, float&)`) which would be discarded because they are not valid UDR prototypes and only `float bar(float, float)` would succeed. So we properly deduce that the user meant `bar<float>`.

*Note:* Needless to say that all this process is not necessary if the user had written `bar<float>` instead of `bar`.

Until here things have not gone wild yet and seem reasonably under control. But, nevertheless, they are not still
as powerful as they might be. So the next logical step are template UDRs.

# C++ with templates

Are templatized UDRs actually needed? It turns that there are cases an extra amount of genericity may be useful. Consider for instance


        #pragma omp declare reduction(template <typename _T> A<_T>::foo : A<_T>)

would declare a UDR for all the types `A<_T>`. The operator would be a function qualified by `A<_T>` called `foo`.

## Added dificulty

Well, for overloads and templates we had to deduce the precise name being used. But, like it usually happens in templated code in C++, we cannot do such checks in template UDRs and delay them until instantiation.

So a templated UDR (and an UDR used in a dependent contexts, examples below) cannot be checked and must be delayed until instatiation.


        // These two are templated UDRs
        #pragma omp declare reduction(template <typename _T> A<_T>::foo : A<_T>)
        #pragma omp declare reduction(template <typename _T> _T :: foo : _T)
        
        template <typename _T>
        struct B
        {
          // This one happens in a dependent context although not a template UDR
          #pragma omp declare reduction (B<_T>::bar : B<_T>)
        };

While this makes sense, the biggest problem happens when we have to 'name' those template UDR. Even if we did not check anything on them (because they were on dependent contexts or templated UDRs) the user will refer them causing a UDR be
'instantiated'. This is particularly important for templated UDRs (not for dependent UDRs will be rendered as normal UDR when the dependent context where they are, a template class or template function, is instantiated).

It is unclear to me when such UDR instantiation should happen. In my opinion it should happen when the UDR is used, as it is shown below (but some people might argue that you may want to give a specialized version, more on this later).


        #pragma omp declare reduction(template <typename _T> _T :: foo : _T)
        
        struct C
        {
          void foo(const C&);
        };
        
        C c;
        
        #pragma omp .. reduction(C::foo : c)
        {
          ...
        }

So we have to figure somehow that this `C::foo` refers to the previously declared UDR. A strategy that works is as follows:

 * Get all the UDRs whose name refers to `foo`. 
 * For each one of those UDR perform an overload using the type-name of the UDR in the following way
   * Given a `pragma omp declare reduction(operator:type)` invent a function `void F(type)` where F is an artificial name not repeated.
   * Apply overload to the set of those artificial functions using the type of the variable in the clause (in the example this is `C`). So we would try to perform an overload of `void F(C)` against `void F<_T>(_T)`, so we deduce the template parameter `_T` of the UDR as being `_T ← C`.
 * Use the deduced template arguments in the operator-name (which since it is a dependent name we do not know anything of it). This will lead us to C::foo, which we discover it is a member function.

Other examples


        #pragma omp declare reduction(template <typename _T> _T :: foo : _T)
        
        struct D
        {
         static D foo(D, D);
         static float foo(float, float);
        };
        
        D d;
        
        #pragma omp .. reduction(D::foo : d)
        {
          ...
        }

In this second example `D::foo` would be found to be a nonstatic member function (so it behaves like a normal function). Compiler work has not finished yet, we have to find the proper function in case it was overloaded (like in this second example). Associativity info of the template UDR would be used to check that everything is correct (so if the templated UDR stated a left associativity but the deduced name was not eligible for such associativity an error would arise).

## Specializations

All the fuss about overload in the previous section comes to solve cases like this


        #pragma omp declare reduction(template <typename _T> A<_T> :: foo : A<_T>)
        #pragma omp declare reduction(template <typename _T> A<_T*> :: foo : A<_T*>)
        
        A<int> a;
        A<int*> b;
        
        #pragma omp ... reduction(A<int>::foo : a) reduction(A<int*>::foo : b)
        {
        }

or if the dot notation is accepted


        // (1)
        #pragma omp declare reduction(template <typename _T> A<_T> :: foo : A<_T>)
        // (2)
        #pragma omp declare reduction(template <typename _T> A<_T*> :: foo : A<_T*>)
        
        A<int> a;
        A<int*> b;
        
        #pragma omp ... reduction(.foo : a, b)
        {
        }

For variable `a` we would deduce the UDR `(1)` because overload would only match that one (we would be deducing the template arguments from F(A<int>) against F(A<_T>) and F(A<_T*>), obviously the latter will be discarded). For variable `b` we would deduce the UDR `(2)` because overload would choose that one as the most defined function (we would be comparting F(A<int*>) against F(A<_T>) and F(A<_T*>) yielding two deductions, respectively, `[T <- int*]` and `[T <- int]`, being the latter the most defined one).