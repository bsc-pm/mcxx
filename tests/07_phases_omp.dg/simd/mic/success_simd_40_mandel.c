/*
<testinfo>
test_CFLAGS="--only-adjacent-accesses"
test_generator=config/mercurium-parallel-simd-mic
</testinfo>
*/

#include <stdio.h>

#pragma omp simd uniform(c_im, count) 
int mandel(float c_re, float c_im, int count)
{
    float z_re = c_re, z_im = c_im;
    int i;
    for (i = 0; i < count; ++i)
    {
        float z_re2 = z_re*z_re;
        float z_im2 = z_im*z_im;

        if (z_re2 + z_im2 > 4.0f)
            break;
            
        float new_re = z_re2 - z_im2;
        float new_im = 2.0f * z_re * z_im;
        z_re = c_re + new_re;
        z_im = c_im + new_im;
    }
    
    return i;
}


void mandelbrot_simd(
        float x0,  float y0, 
        float x1,  float y1,
        int width,  int height, 
        int maxIterations,
        int output[])
{
    float dx = (x1 - x0) / width;
    float dy = (y1 - y0) / height;

    for (int j = 0; j < height; j++)
    {
        float y = y0 + j * dy;

        int i;

#pragma omp simd for 
        for(i = 0; i < width; i++)
        {
            float x = x0 + i * dx;
            output[j * width + i] = mandel(x, y, maxIterations);
        }
    }
}

void mandelbrot_sc(
        float x0,  float y0, 
        float x1,  float y1,
        int width,  int height, 
        int maxIterations,
        int output[])
{
    float dx = (x1 - x0) / width;
    float dy = (y1 - y0) / height;

#pragma novector
    for (int j = 0; j < height; j++)
    {
        int i;
#pragma novector     
        for(i = 0; i < width; i++)
        {
            float x = x0 + i * dx;
            float y = y0 + j * dy;
//            int index = j * width + i;
//            output[index] = mandel(x, y, maxIterations);

            float z_re = x; 
            float z_im = y;
            int k;
#pragma novector
            for (k = 0; k < maxIterations; ++k)
            {
                float z_re2 = z_re * z_re;
                float z_im2 = z_im * z_im;

                if (z_re2 + z_im2 > 4.0f)
                    break;

                float new_re = z_re2 - z_im2;
                float z_mul = z_re * z_im;
                float new_im = z_mul * 2.0f; //+ z_mul;
                z_re = x + new_re;
                z_im = y + new_im;
            }

            //output[index] = k;
            output[j * width + i] = k;
        }
    }
}


int main(int argc, char* argv[])
{
    unsigned int width = 640;
    unsigned int height = 480;
    unsigned int maxIterations = 100;
    unsigned int iters = 1;
    float x0 = -2.0f, x1 = 1.0f;
    float y0 = -1.0f, y1 = 1.0f;
    
    int *buf_simd;;
    if(posix_memalign((void **) &buf_simd, 64, width * height * sizeof(int)) != 0)
    {
        exit(1);
    }

    int *buf_sc = (int *) malloc(width * height * sizeof(int));

#pragma omp parallel firstprivate(iters)
    {
        int i;

        for(i=0; i<iters; i++)
            mandelbrot_simd(x0, y0, x1, y1, width, height, maxIterations, buf_simd);
    }

    mandelbrot_sc(x0, y0, x1, y1, width, height, maxIterations, buf_sc);
 
    for (int i=0; i<height; i++)
    {
        int j;
        for (j=0; j<width; j++)
        {
            int simd_val = buf_simd[i * width + j]; 
            int sc_val = buf_sc[i * width + j]; 

            if (simd_val != sc_val)
            {
                fprintf(stderr, "[%d,%d] -> %d != %d\n", i, j, sc_val, simd_val);
                return 1;
            }
        }
        //printf("\n");
    }

    fprintf(stderr, "Success!\n");
    return 0;
}
