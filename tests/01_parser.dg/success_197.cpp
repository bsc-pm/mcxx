/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct CvDTreeSplit
{
    int var_idx;
    int condensed_idx;
    int inversed;
    float quality;
    CvDTreeSplit* next;
    union
    {
        int subset[2];
        struct
        {
            float c;
            int split_point;
        }
        ord, ord2;
        int x;
    };
};

struct dispatch_job_t {
   enum dispatch_type_e {JOB_SCORE} type;
   union {
      struct {
         int score;
         int bx;
         int by;
      } score;
   };
};
