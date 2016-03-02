# Dependences by value

## Overview

We aim at supporting _dependences by value_ to be (only) *input*


        #pragma omp task input(x) output(*y)
        void f(int x, int *y)
        {
          *y = x + 1;
        }
        
        void g(void)
        {
          int a, b, c;
          a = 1;
        
          f(a, &b);
          f(b, &c);
        #pragma omp taskwait on(c)
        }

When the argument of the value dependence is an lvalue, the behaviour
should be as shown below.


        #pragma input(*x) output(*y)
        void f1(int *x, int *y)
        {
          *y = *x + 1;
        }
        
        void g(void)
        {
          int a, b, c;
          a = 1;
        
          int *tmp0 = &a;
          f1(tmp0, &b);
        
          int *tmp1 = &b;
          // This task will not be run until the previous f1 task has ended
          f1(tmp1, &c); 
        #pragma omp taskwait on(c)
        }

When the argument of the value dependence is an rvalue, then
the behaviour should be as follows


        // Note that now there is no input(x), so x is captured
        // at the task creation
        #pragma omp task output(*y)
        void f2(int x, int *y)
        {
          *y = x + 1;
        }
        
        void g(void)
        {
          int a, b, c;
          a = 1;
        
          f2(a, &b);
        
          // This task MAY run in parallel to the previous f2
          f2(b, &c);
        #pragma omp taskwait on(c)
        }

## Another related example


        #pragma omp task inout(*i)
        void task_generates(int *i);
        
        #pragma omp task input(i)
        void code_uses(int i);
        
        void f()
        {
            int i = ...;
            for (i = 0; i < 100; i++)
            {
                if (...)
                {
                   task_generates(&i);
                }
                else
                {
        #pragma omp taskwait on(i)
                   i = i + 1;
                }
        // Instead of a task function call this could be an inline task with input(i)
                code_uses(i);
            }
        }

## Difficult cases


        #pragma omp task input(x) output(*y)
        void f(int x, int *y)
        {
          *y = x;
        }
        
        void g(void)
        {
          int a[100];
          int i;
        
          for (i = 0; i < 100; i++)
          {
            f(i, &a[i]);
          }
          // -- Let's assume no task has got a chance to run until here!!! ---
          
          #pragma omp taskwait on(a[99])
        }

In this case all tasks will see a value of `i` of 100 since it is an lvalue.

=== Solution 1: unary + === 

Turn argument into rvalue by using unary `+`


          for (i = 0; i < 100; i++)
          {
            f(+i, &a[i]);
          }

This is a weird syntax and only works for arithmetic types. It may also cause unexpected casts if the argument is a type with lower rank than int (char and short).

### Solution 2: ompss_nodep

This is inspired by `std::move` of C++2011 (which converts a `T&` into a `T&&` when such cases arise). ompss_nodep would be an intrinsic in Mercurium which would perform an implicit lvalue → rvalue conversion, thus the argument
would not be considered an lvalue anymore.


          for (i = 0; i < 100; i++)
          {
            f(ompss_nodep(i), &a[i]);
          }

This is a bit weird, as well.

### Solution 3: assume both cases may be useful and let the runtime choose

 1. The compiler will allocate a temporary storage for the variable in the dependence along with the usual pointer kept in the arguments structure.
 2. The compiler will initialize the pointer
  * If it is a rvalue or *lvalue without pending writers* it will point to the temporary storage and copy the value of the program entity at this point.
  * If it is a *lvalue with pending writers* the pointer will point to the program entity. The temporary storage need not be initialized (it should not be used either)

It may happen that the pending writers are released between asking the runtime and submitting the task. This is not a problem actually. It cannot happen that a pending writer is held after asking the runtime as long as only one thread is creating tasks (otherwise this would create a race condition).

This solution seems sound although it may slightly increase the overhead of task creation. It should not be much worse than a `firstprivate`.