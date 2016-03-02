# UDR Extended identity



        <p style="color: red; font-size: 16pt;">This information is outdated and kept here only for historical references. These proposals have been deprecated after OpenMP 4.0 declare reduction syntax.</p>



We extend the syntax of declare reduction with an optional id-name


        #pragma omp declare reduction (op-name-list : type-seq : id) identity(identity-expression)

`identity-expression` is an identity expression (as already specified in the OpenMP UDR draft) which can contain an `id` where
an _identifier_ can appear. `id` shall be a valid name for an identifier.

The _declarative region_ (see C++ Standard §3.3) of `id` is restricted to the region enclosed by the parentheses after `identity` token.
The `identity-expression` is considered for each type `T` in `type-seq`. The type of `id` is declared to  be 
an object of type `T`.


## Alternative syntax

Propose here alternative syntax for the location/specification of `id`.


        identity(id : identity-expression)