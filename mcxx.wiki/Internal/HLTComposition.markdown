[[PageOutline]]

# HLT Composition

## Related Work

TBD

## Summary

|        | Unroll | Fusion | Task | For |
|--------|--------|--------|------|-----|
| Unroll |   -    |   -    | Unroll+aggregate tasks | |
| Fusion |  jam   |   -    | Fusion+aggregate tasks | |
| Task   |   -    |   -    |  -   |  -  |
| For    |        |        |      |     |

## Composition within HLT

### unroll + fusion

This is is equivalent too unroll+jam. 

## Composition with OpenMP

### Task + Unroll


        #!cpp
        #pragma omp task
        #pragma hlt unroll
        for (…)

remains the same.

### Unroll + Task

The loop is unrolled, but we try to generate still as many tasks as in the original by aggregatting loop bodies into them.
The epilog has tasks as in the original.


        #!cpp
        #pragma hlt unroll factor(2)
        for (…)
        #pragma omp task
           A 
becomes 

        #!cpp
        #pragma hlt unroll factor(2)
        for (…)
        #pragma omp task
        { A; A }

This gets more complicated when the loop body has code interleaved with tasks.


        #!cpp
        #pragma hlt unroll factor(2)
        for (…)                 
        {
           A
        #pragma omp task
           B
        #pragma omp task
           C
           D
        }

would become


        #!cpp
        #pragma hlt unroll factor(2)
        for (…)                 
        {
           A1; D1; A2; D2
        #pragma omp task
           { B1; B2 }
        #pragma omp task
           { C1 ; C2 }
        
           Commit(A2,D2)
        }

This requires some analysis and the capacity to remove interloop dependencies by using temporal values.

Shall we support also, currently illegal, but probably useful unroll over task directly?


        #!cpp
        #pragma hlt unroll
        #pragma omp task
        for (...)

### Task + Fusion

Nothing special happens

### Fusion + Task

In general, we want to join the loops and maintain the number of tasks per iterations about the same. Which means to try to group tasks together.

For example:


        #!cpp
        #pragma hlt fusion
        {
        for (...)
          #pragma omp task
             A
        for (...)
          #pragma omp task
             B
        }

becomes


        #!cpp
        #pragma hlt fusion
        {
        for (...)
          #pragma omp task
          { A; B }
        }

Because there may be non-task code, we probably need to do a transformation similar to the unroll+task one.

Also, support


        #!cpp
        #pragma hlt fusion
        #pragma omp task
        {
        for (...)
        for (...)
        }


## Composition with Ss