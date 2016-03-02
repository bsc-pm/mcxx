
 *Note:* While most of the examples focus on loops nothing prevents us from using these coallescing strategies in
plain sequential (non iterative) codes where tasks are created within if-statements.


Given something like


        #!cpp
        #pragma hlt unroll factor(4)
        for (jj=kk+1; jj<bots_arg_size; jj++)
        {
           if (BENCH[kk*bots_arg_size+jj] != NULL)
              #pragma omp task untied firstprivate(kk, jj) shared(BENCH)
           {
              fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+jj]);
           }
        }

we can "unroll" it.

## Predication


        #!cpp
        for (jj=kk+1; jj<(bots_arg_size-3); jj += 4)
        {
            // These can be bits instead of chars
            char c1, c2, c3, c4;
        
            c1 = (BENCH[kk*bots_arg_size+jj] != NULL);
            c2 = (BENCH[kk*bots_arg_size+(jj + 1)] != NULL);
            c3 = (BENCH[kk*bots_arg_size+(jj + 2)] != NULL);
            c4 = (BENCH[kk*bots_arg_size+(jj + 3)] != NULL);
        
            if (c1 || c2 || c3 || c4)
            {
        #pragma omp task firstprivate(c1, c2, c3, c4, kk, jj)
                {
                    c1 && (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+jj]));
                    c2 && (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(jj + 1)]));
                    c3 && (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(jj + 2)]));
                    c4 && (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(jj + 3)]));
                }
            }
        }
        
        // Tail code goes here!

Pros:

 * Takes advantage of predicated architectures (not many, the truth be told)
 * Does not increase overall complexity of the task creator code
 * Task code dynamically adapts its granularity
 * Predicates can be passed as bits, so packing them very efficiently 
   and not meaning too much additional data to the task

Cons:

 * Need to compute bits for predication
 * Granularity is not constant among tasks: *they may be unbalanced*
 * Tasks, in non predicated architectures, will have one branch per task (eventually).

## Bundling


        #!cpp
        int vec_jj[4];
        int _index = 0;
        
        for (jj=kk+1; jj<(bots_arg_size-3); jj += 4)
        {
            // Fes-ho amb un bit :D
            char c1, c2, c3, c4;
        
            if(BENCH[kk*bots_arg_size+jj] != NULL)
            {
                vec_jj[_index] = jj;
                _index++;
                if (_index == 4)
                {
        #pragma task firstprivate(vec_jj, vec_kk)
                    {
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+vec_jj[0]]));
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(vec_jj[1])]));
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(vec_jj[2])]));
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(vec_jj[3])]));
                    }
        
                    _index = 0;
                }
            }
            if(BENCH[kk*bots_arg_size+(jj + 1)] != NULL)
            {
                vec_jj[_index] = jj + 1;
                _index++;
                if (_index == 4)
                {
        #pragma task firstprivate(vec_jj, vec_kk)
                    {
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+vec_jj[0]]));
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(vec_jj[1])]));
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(vec_jj[2])]));
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(vec_jj[3])]));
                    }
        
                    _index = 0;
                }
            }
            if(BENCH[kk*bots_arg_size+(jj + 2)] != NULL)
            {
                vec_jj[_index] = jj + 2;
                _index++;
                if (_index == 4)
                {
        #pragma task firstprivate(vec_jj, vec_kk)
                    {
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+vec_jj[0]]));
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(vec_jj[1])]));
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(vec_jj[2])]));
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(vec_jj[3])]));
                    }
        
                    _index = 0;
                }
            }
            if(BENCH[kk*bots_arg_size+(jj + 3)] != NULL)
            {
                vec_jj[_index] = jj + 3;
                _index++;
                if (_index == 4)
                {
        #pragma task firstprivate(vec_jj, vec_kk)
                    {
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+vec_jj[0]]));
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(vec_jj[1])]));
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(vec_jj[2])]));
                        (fwd(BENCH[kk*bots_arg_size+kk], BENCH[kk*bots_arg_size+(vec_jj[3])]));
                    }
        
                    _index = 0;
                }
            }
        }
        
        // Tail code goes here

Pros

 * Created tasks benefit from maximum (chosen) granularity (except for the last bundle, of course)

Cons

 * More bookkeeping required and created tasks require more data. The additional cost of passing larger data *might hinder any performance gain*
 * To reduce data requirements of tasks, one must know invariant firstprivates. Using a pragma may be helpful