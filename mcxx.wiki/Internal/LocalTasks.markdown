# Local Tasks

## Definitions

A _local bounded task_ (in lack for a better name), LBT for short, is a task that references a local variable (i.e. a variable the lifetime of which is the same as a function activation). This is a property of the program (and may be or may not be determined at compile time). If a task can be determined to be LBT at compile time we will call it static LBT (SLBT).


        
        Example
        
        {{{
        #!cpp
        void fact(int x)
        {
          if (x > 0)
          {
            int y;
            #pragma omp task in(x) out(y)
            {
              y = x * fact(x - 1);
            }
        #pragma omp taskwait on(y)
            return y;
          }
          else return 1;
        }

The created task is a LBT as it uses the local variables `x` and `y`.
}}}

A _late-executed task_, LET for short, is a task that in an execution runs after the activation of the function that created it. This is a property of the execution (or schedule). This is only known at runtime.


        
        Example
        
        {{{
        #!cpp
        void traversal(binary_tree* b, void (*process)(binary_tree_node* n))
        {
          if (b == NULL)
            return;
        
          // (A)
          #pragma omp task firstprivate(b)
          { 
            process(b->node);
          }
        
          traversal(b->right);
          traversal(b->left);
        }

The task at `(A)` may potentially run after the invocation of `traversal` that created it. If this happens
that task is LET.
}}}

## Guiding principle

*In a correct execution, no LBT task should ever be scheduled in runtime as LET.*

*At runtime we can warn the user if a SLBT is going to be run as LET* (and possibly take corrective measures like a taskwait or a similar synchronization).

## More definitions

A potentially late-executed task (PLET) is a task that may or may not be LET. This is a property of the program and may (or may not) be determined at compile-time.


        
        {{{
        #!cpp
        void f(int x, int y)
        {
          #pragma omp task firstprivate(x)
          {
            ...
          }
          if (y > 3)
          {
        #pragma omp taskwait
          }
        }

It may happen that the created task is run LET, but maybe it is never run as such because `y` is always greater than 3 at runtime.

}}}

If a task can be determined at compile time to be PLET then we say it is a static PLET (SPLET).

## Guiding principles

*At compile time we can warn the user if a SPLET is SLBT*

## Other considerations

Given that SLBT is a subset of LBT and SPLET is a subset of PLET, it is the quality of the analyis what determines how large is such subset. The idea is avoid false positives at all cost and let the quality of the analysis determine the amount of false negatives.