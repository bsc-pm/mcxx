## Outline
Given a set of statements (most of the time from a compound  statement) it is possible to generate an 'outline' function that  contains these statements and receives the needed parameters.


        #pragma hlt outline name(outline-name)
          statement
Unfortunately there is currently no way to tell from pragmas  how are the parameters going to be passed to the outlined function.  Clause `name` can be used to override the name of the outlined function.

## Function extension
Given a function definition we may want to expand it to take array versions of its inputs.


        #pragma hlt expand factor(expr) name(expanded-fun-name)
        function-definition
If clause `factor` contains a constant expression number  of parameters will remain the same, their types will be array extended.  If it is not a constant value, an initial integer parameter with the  size of the array must be passed to the expanded function. Use clause `name` to override the name of the expanded function.

This pragma directive cannot be applied to functions with zero parameters or ellipsis.