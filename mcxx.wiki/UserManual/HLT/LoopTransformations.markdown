## Loop unrolling
Given a regular loop we can unroll it a given amount of times using


        #pragma hlt unroll factor(N)
          for-stmt
where `N` is a constant-expression.

Example,


        #pragma hlt unroll factor(24)
        for (i = 0; i < 100; i++)
        {
          a[i] = i + 1;
        }
## Loop blocking
Given a regular loop we can apply blocking on it using


        #pragma hlt block factors(N)
          for-stmt
Example,


        #pragma hlt block factor(24)
        for (i = 0; i < 100; i++)
        {
          a[i][k] = i + 1;
        }
It is possible to block a nest of loops at a time. Just add as many factors as many nesting levels you want to block. Example,


        #pragma hlt block factors(24, 48)
        for (i = 0; i < 1000; i++)
        {
          for (j = 0; j < 1000; j++)
          {
           a[i][j] = i * j;
          }
        }
        
The resulting code will be a four level nested loop that operates on blocks of 24x48 elements.

## Loop interchange
Given a perfect loop nest we may interchange relative position of loops within the nest.


        #pragma hlt interchange permutation(p{1,..,n})
        
where `n` is the level of the loop nest and `p{1,..,n}` represents a permutation of the sequence of integers from 1 to `n`.

Example,


        #pragma hlt interchange permutation(1,3,2)
        for (i = 0; i < N; i++)
          for (j = 0; j < N; j++)
           for (k = 0; k < N; k++)
              c[i][j] = a[i][k] * b[k][j];
        
## Loop collapse
Given a perfect nest of regular loops we may want to create a single  loop that iterates over the n-dimensional iteration space. Example,


        #pragma hlt collapse
        for (i = 0; i < 100; i++)
          for (j = 0; j < 200; j++)
           for (k = 0; k < 300; k++)
             a[i][j][k] = i * j * k;
        
## Loop fusion
Given a sequence of loops sharing the same iteration space we may want to fuse them in a single loop.


        #pragma hlt fusion
        {
           for-statement ...
        }
        
Example


        #pragma hlt fusion
        {
          for (i = 0; i < 100; i++)
            a[i] = i + 1;
          for (i = 0; i < 100; i++)
            b[i] = i + 2;
        }
        
## Loop distribution
Loop distribution can be understood as the opposite of loop fusion.  Every statement in the loop body will be contained in its own loop. It  is possible to perform array expansion for intra loop dependences using  clause `expand`.


        #pragma hlt distribute expand(var-list)
           for-stmt
        
Example,


        
        int t;
        #pragma hlt distribute expand(t)
        for (i = 0; i < 100; i++)
        {
          t = a[i];
          b[i] = t * t;
        }
        
in this example `t` would be expanded as an array and replaced in every iteration as `t[i]`.