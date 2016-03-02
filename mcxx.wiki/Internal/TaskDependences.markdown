# Task Dependencies

## Representant and Data


        #!cpp
        
        data *buffer = malloc(buffersize); // check for NULL left to stage tasks
        for(i=0; true; ++i) {  
          ;
          #pragma omp task output([buffersize] buffer)
            stage1(buffer);
        
          #pragma omp task inout([buffersize] buffer)
            stage2(buffer);
        
          #pragma omp task inout([buffersize] buffer)
            stage3(buffer);
        
          #pragma omp task input([buffersize] buffer)
            stage4(buffer);
        }
        free(buffer);


        #!cpp
        
        int sparseLU() {
           int ii, jj, kk;
        
        for (kk=0; kk<NB; kk++) {
        #pragma omp task inout([NB][NB] A[kk][kk])
           lu0(A[kk][kk]);
           for (jj=kk+1; jj<NB; jj++)
              if (A[kk][jj] != NULL)
        #pragma omp task input([NB][NB] A[kk][kk]) inout([NB][NB] A[kk][jj])
                 fwd(A[kk][kk], A[kk][jj]);
        
           for (ii=kk+1; ii<NB; ii++) 
              if (A[ii][kk] != NULL) {
        #pragma omp task input([NB][NB] A[kk][kk]) inout([NB][NB] A[ii][kk])
                 bdiv (A[kk][kk], A[ii][kk]);
                 for (jj=kk+1; jj<NB; jj++)
                    if (A[kk][jj] != NULL) {
                       if (A[ii][jj]==NULL) A[ii][jj]=allocate_clean_block();
        #pragma omp task input([NB][NB] A[ii][kk], [NB][NB] A[kk][jj]) inout([NB][NB] A[kk][kk])
                       bmod(A[ii][kk], A[kk][jj], A[ii][jj]);
                    }
              }
        }
        

## Only representant (variable)



        #!cpp
        
        
        for(i=0; true; ++i) {  
          data *buffer = malloc(buffersize); // check for NULL left to stage tasks
          #pragma omp task output(buffer)
            stage1(buffer);
        
          #pragma omp task inout(buffer)
            stage2(buffer);
        
          #pragma omp task inout(buffer)
            stage3(buffer);
        
          #pragma omp task input(buffer)
          {
            stage4(buffer);
            free(buffer)
          }
        }
        free(buffer);



        #!cpp
        
        int sparseLU() {
           int ii, jj, kk;
        
        for (kk=0; kk<NB; kk++) {
        #pragma omp task inout(A[kk][kk])
           lu0(A[kk][kk]);
           for (jj=kk+1; jj<NB; jj++)
              if (A[kk][jj] != NULL)
        #pragma omp task input(A[kk][kk]) inout(A[kk][jj])
                 fwd(A[kk][kk], A[kk][jj]);
        
           for (ii=kk+1; ii<NB; ii++) 
              if (A[ii][kk] != NULL) {
        #pragma omp task input(A[kk][kk]) inout(A[ii][kk])
                 bdiv (A[kk][kk], A[ii][kk]);
                 for (jj=kk+1; jj<NB; jj++)
                    if (A[kk][jj] != NULL) {
                       if (A[ii][jj]==NULL) A[ii][jj]=allocate_clean_block();
        #pragma omp task input(A[ii][kk], A[kk][jj]) inout(A[kk][kk])
                       bmod(A[ii][kk], A[kk][jj], A[ii][jj]);
                    }
              }
        }
