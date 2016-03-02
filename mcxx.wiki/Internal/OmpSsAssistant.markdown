
 Suggest alternate constructions that favour the malleability and help eradicating bad practices that plague OpenMP.

[ This idea is still evolving so it is probably incomplete/incorrect ]

## Motivation

This is both for OmpSs and OpenMP (although we will obvious favour the former).

Consider the following example


        #pragma omp parallel
        {
            int x; // effectively creates a threadprivate var
        #pragma omp for
            for ( ... )
            {
            }
        }

conceptually a threadprivate var can be seen as a map thread-id -> var. Ideally
the map may be dynamic in runtime should a new thread
"appear". This is what more or less pthreads achieve with Thread Local Storage
variables, but again this can only happen upon creation of the pthread: if the
pthread is pooled the variables will not be created again(need to confirm this case, maybe
the TLS storage can be reinitialized???).

In general threadprivate variables implemented using TLS are not inefficient but lead to a SPMD-style
to OpenMP. If they are implemented using a function that solves the map, they may be inefficient although
compiler analysis can mitigate some of the cost (one does not usually need evaluate several more than once the existence
of a variable).

Note: In OmpSs `parallel` is ignored, although the following examples will feature
parallel constructs to show whether the case applies to OpenMP as well.

## Study of cases

(Note, there may be more cases, feel free to add them)

### Variables that may be moved from threadprivate to (plain) private


        #pragma omp parallel
        {
            int x;
        #pragma omp for
            for ( ... )
            {
                // x is dead at the end of each iteration
                // (i.e. it is not used before its first definition)
            }
        }

This case does not usually happen in C where moving the declaration inside the `for` is enough, but in Fortran (where this is not possible before Fortran 2008).

In this case the compiler could suggest the following transformation


        #pragma omp parallel
        {
        #pragma omp for private(x)
            for ( ... )
            {
                // x is dead at the end of each iteration
                // (i.e. x it is not used before its first definition)
            }
        }

Cons: overhead, 'x' may be an array or a big structure with an expensive copy operation.

Pro: we have effectively created a combinator (i.e. code without free variables). This is good and
gives us much more freedom in terms of malleability.

### Variables the size of which depends on the size of the next team


        int x[omp_get_max_threads()];
        
        #pragma omp parallel
        {
        #pragma omp for
            for ( ... )
            {
                int current_thread = omp_get_thread_num();
                x[current_thread] = ...;
                ... = x[current_thread];
            }
        }

This works in OmpSs but it is undesirable if we are interested in changing the
threads during the execution of the workshare.

If the variable 'x' is dead at every iteration and the values "escape" from
one thread to another (i.e. the thread-dependent expression is always the value of
omp_get_thread_num()) the 'x' can easily be privatized.

If the variable 'x' is not dead, then this is some sort of reduction per
thread. It would be great to suggest to use a reduction construct here: UDRs
may prove useful. Unfortunately UDRs cannot have state so a general approach
may not be feasible: if the "reduction" depends on the value of some other
variable other than omp_in, omp_out. For example, imagine we only reduce the
even elements for some reason so there is an if-statement protecting the even
case: the evaluated predicate p (where p = is_even(x)) should ideally be passed
to the combiner, which would use it to gate the reduction itself. Note that
some cases may also imply that the code is doing a "nonassociative reduction",
these kind of reductions cannot be reordered so running them in parallel is
likely unfeasible. This case is hard but may be very interesting for
(associative) reductions: OpenMP may need combiners to be extended, analysis
may allow synthesizing (with limitations, of course) the combiner.

If the variable 'x' escapes from one thread to another, this is such a weird
code, probably sprinkled with `atomic`, `critical` and `flush`. There is little hope
with such code. We expect them to be rare.

## Ultimate goal

We need to define the concept of _static malleability_ where the malleability as such is only possible
ahead of time (before executing the program or in a more fine grained p.o.v before a parallel construct).

As a consequence we aim for _dynamic_ malleability, where such constraint does not exist and we can change
the number of threads on demand during the program.

## Compiler analysis

Using classical dataflow analysis (and the PCFG in the compiler) we can tag expressions as:

 * _thread-dependent_ where the value of the expression is positively known to be some function of `omp_get_thread_num()`
 * _futurely-team-constrained_ where the value of the expression is positively known to be some function of `omp_get_max_threads()`
 * _currently-team-constrained_ where the value of the expression is positively known to be some function of `omp_get_num_threads()`

Note that _positively_ above means that not having such tag does not imply that the expression does not have such property, it may happen that the compiler could not prove it.

Using this tags we can classify arrays as _{futurely,currently}-team-constrained-size_ if the expressions that determine their size (and this may imply analyzing `ALLOCATE` in Fortran) are _{futurely,currently}-team-constrained_. Accesses to such arrays have to be analyzed to see if they are indexed through _thread-dependent_ expressions.

Nowe we should be able to combine this knowledge with classical liveness analysis so we can classify the codes in one of the cases shown above.