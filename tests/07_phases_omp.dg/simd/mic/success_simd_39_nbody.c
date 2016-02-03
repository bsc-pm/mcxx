/*
<testinfo>
test_CFLAGS="--only-adjacent-accesses --only-aligned-accesses"
test_generator=config/mercurium-parallel-simd-mic
</testinfo>
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <assert.h>
#include <unistd.h>
#include <sys/stat.h>

static const float domain_size_x = 1.0e+10;
static const float domain_size_y = 1.0e+10;
static const float domain_size_z = 1.0e+10;
static const float G = 6.6726e-11;
static const float time_interval = 1.0e+0;
static const float max_mass = 1.0e+28;
static const int seed = 12345;

#define bs 512

typedef struct //particle_t
{
  float x;
  float y;
  float z;
  float vx;
  float vy;
  float vz;
  float m;
  float Gm; //weight
} particle_t;

typedef struct
{
  float x;
  float y;
  float z;
} force_t;

typedef struct //particle_t
{
  float x[bs];
  float y[bs];
  float z[bs];
  float vx[bs];
  float vy[bs];
  float vz[bs];
  float m[bs];
  float Gm[bs]; //weight
} soa_particle_t;

typedef struct
{
  float x[bs];
  float y[bs];
  float z[bs];
} soa_force_t;

#define QUAL const //__restrict

static void init_particle_soa(float* QUAL x, float* QUAL y, float* QUAL z, float* QUAL m, float* QUAL Gm)
{
  *x  = domain_size_x * ((float)rand() / ((float)RAND_MAX + 1.0));
  *y  = domain_size_y * ((float)rand() / ((float)RAND_MAX + 1.0));
  *z  = domain_size_z * ((float)rand() / ((float)RAND_MAX + 1.0));
  *m  = max_mass * ((float)rand() / ((float)RAND_MAX + 1.0));
  *Gm = G * (*m);
}

static void init_particle_bs(soa_particle_t * QUAL particles)
{
  int i;
  for (i = 0; i < bs; i++) {
    init_particle_soa(&particles->x[i], &particles->y[i], &particles->z[i], &particles->m[i], &particles->Gm[i]);
  }
}

static void init_particles(soa_particle_t* QUAL particles, const int num_particles)
{
  srand(seed);

  int i;
  for (i = 0; i < num_particles/bs; ++i) {
    init_particle_bs(&particles[i]);
  }
}

#pragma omp simd uniform(px, py, pz, pvx, pvy, pvz, pm, fx, fy, fz) linear(i) aligned(px, py, pz, pvx, pvy, pvz, pm, fx, fy, fz)
void __attribute__((noinline)) update_particle_soa(float* QUAL px, float* QUAL py, float* QUAL pz, 
                                float* QUAL pvx, float* QUAL pvy, float* QUAL pvz,
                                float* QUAL pm, //float* QUAL pGm, 
                                float* QUAL fx, float* QUAL fy, float* QUAL fz, int i)
{
  const float m = pm[i];
  const float vx = pvx[i];
  const float vy = pvy[i];
  const float vz = pvz[i];

  const float x = px[i];
  const float y = py[i];
  const float z = pz[i];
  const float t_div_m = time_interval / m;

  const float half_time_interval = 0.5f * time_interval;

  const float vx_change = fx[i] * t_div_m;
  const float vy_change = fy[i] * t_div_m;
  const float vz_change = fz[i] * t_div_m;

  const float x_change = vx + vx_change * half_time_interval;
  const float y_change = vy + vy_change * half_time_interval;
  const float z_change = vz + vz_change * half_time_interval;

  pvx[i] = vx + vx_change;
  pvy[i] = vy + vy_change;
  pvz[i] = vz + vz_change;

  px[i] = x + x_change;
  py[i] = y + y_change;
  pz[i] = z + z_change;
}

void update_particle_bs(soa_particle_t * QUAL particles, soa_force_t * QUAL forces)
{
  int i;

  float * QUAL px = particles->x;
  float * QUAL py = particles->y;
  float * QUAL pz = particles->z;
  float * QUAL vx = particles->vx;
  float * QUAL vy = particles->vy;
  float * QUAL vz = particles->vz;
  float * QUAL m = particles->m;
  float * QUAL fx = forces->x;
  float * QUAL fy = forces->y;
  float * QUAL fz = forces->z;

#pragma omp simd aligned(px, py, pz, vx, vy, vz, m, fx, fy, fz)
  for (i = 0; i < bs; i++)
    update_particle_soa(px, py, pz,
                        vx, vy, vz,
                        m,  //particles->Gm,
                        fx, fy, fz, i);
}
#pragma omp simd uniform(p1x, p1y, p1z, p1m, p2x, p2y, p2z, p2m, j, l) linear(fx, fy, fz:4) linear(i, k:1) aligned(fx, fy, fz, p1x, p1y, p1z, p1m, p2x, p2y, p2z, p2m)
void __attribute__((noinline)) calculate_force_soa(float* QUAL p1x, float* QUAL p1y, float* QUAL p1z, 
                                float* QUAL p1m,
                                float* QUAL p2x, float* QUAL p2y, float* QUAL p2z, 
                                float* QUAL p2m, 
                                float* QUAL fx, float* QUAL fy, float* QUAL fz,
                                int i, int j, int k, int l)
{
  const float diff_x = p2x[l] - p1y[k];
  const float diff_y = p2y[l] - p1y[k];
  const float diff_z = p2z[l] - p1z[k];
  const float dist_sqr = diff_x * diff_x + diff_y * diff_y + diff_z * diff_z;
  //const float dist = sqrtf(dist_sqr);
  const float inv_dist = 1.0f / sqrtf(dist_sqr);

  const float m1 = p1m[k];
  const float m2 = p2m[l];

  //  float f = G * ((m1 * m2) / (dist_sqr * dist));
  float f = (((m1*inv_dist)/dist_sqr) * (m2 * G));

  if (j == i)
    f = 0.0f;

  *fx += f * diff_x;
  *fy += f * diff_y;
  *fz += f * diff_z;
}

void calculate_force_bs(soa_particle_t *QUAL particle1, soa_particle_t * QUAL particle2,
                                soa_force_t * QUAL forces, const int i, const int j)
{
  int k, l;

  float * QUAL p1x = particle1->x;
  float * QUAL p1y = particle1->y;
  float * QUAL p1z = particle1->z;
  float * QUAL p1m = particle1->m;
  float * QUAL p2x = particle2->x;
  float * QUAL p2y = particle2->y;
  float * QUAL p2z = particle2->z;
  float * QUAL p2m = particle2->m;
  float * QUAL fx = forces->x;
  float * QUAL fy = forces->y;
  float * QUAL fz = forces->z;

#pragma omp simd aligned(p1x, p1y, p1z, p1m, p2x, p2y, p2z, p2m, fx, fy, fz) 
  for (k = 0; k < bs; k++) {
    float x = fx[k];
    float y = fy[k];
    float z = fz[k];

    for (l = 0; l < bs; l++) {
      calculate_force_soa(p1x, p1y, p1z,
                          p1m, //p1Gm,
                          p2x, p2y, p2z,
                          p2m, //p2Gm,
                          &x, &y, &z, i + k, j + l, k, l);
    }

    fx[k] = x;
    fy[k] = y;
    fz[k] = z;
  }
}

static void calculate_nbody(soa_particle_t* QUAL particles, soa_force_t* QUAL forces, int num_particles)
{
  int i, j, k, l;

#pragma omp for
  for (i = 0; i < num_particles/bs; ++i) {
    for (j = 0; j < num_particles/bs; j++) {
      calculate_force_bs(&particles[i], &particles[j], &forces[i], i * bs, j * bs);
    }
    update_particle_bs(&particles[i], &forces[i]);
  }

//#pragma omp for
//  for (i = 0; i < num_particles/bs; ++i) {
//    int j;
//    for (j = 0; j < bs; j++) {
//      forces[i].x[j] = 0;
//      forces[i].y[j] = 0;
//      forces[i].z[j] = 0;
//    }
//  }
}

static void init_forces(soa_force_t* QUAL forces, const int num_particles)
{
  int i;

  for (i = 0; i < num_particles/bs; ++i) {
    int j;
    for (j = 0; j < bs; j++) {
      forces[i].x[j] = 0;
      forces[i].y[j] = 0;
      forces[i].z[j] = 0;
    }
  }
}

static void check_results(soa_particle_t* particles, const int num_particles, const int timesteps)
{
  char ref_file[1024];

  sprintf(ref_file, "nbody-%d-%d.ref", num_particles, timesteps);

  if (access(ref_file, F_OK) != 0)
    return;

  const int fd = open(ref_file, O_RDONLY, 0);
  //  assert(fd >= 0);

  particle_t* ref_particles = mmap(NULL, num_particles * sizeof(particle_t), PROT_READ, MAP_SHARED, fd, 0);

  double error = 0.0;
  int count = 0;
  int i;
  for (i = 0; i < num_particles; ++i) {
    if ((particles[i/bs].x[i%bs] != ref_particles[i].x) ||
	(particles[i/bs].y[i%bs] != ref_particles[i].y) ||
	(particles[i/bs].z[i%bs] != ref_particles[i].z)) {
      error +=
	fabs(((particles[i/bs].x[i%bs] - ref_particles[i].x) * 100.0) / ref_particles[i].x) +
	fabs(((particles[i/bs].y[i%bs] - ref_particles[i].y) * 100.0) / ref_particles[i].y) +
	fabs(((particles[i/bs].z[i%bs] - ref_particles[i].z) * 100.0) / ref_particles[i].z);

      count++;
    }
  }

  munmap(ref_particles, num_particles * sizeof(particle_t));
  close(fd);

  double rel_err = error / (3.0 * count);
  
  if ((count * 100.0) / num_particles > 0.6 ||
      rel_err > 0.0008)
    printf("Relative error[%d] %f\n", count, rel_err);
  else
    printf("Validation ok\n");
}

static void save_results(soa_particle_t* particles, const int num_particles, const int timesteps)
{
  int i, j;

  particle_t* ref = (particle_t*)malloc(sizeof(particle_t) * num_particles);

  for (i = 0; i < num_particles/bs; i++) {
    for (j = 0; j < bs; j++) {
      ref[i*bs + j].x  = particles[i].x[j];
      ref[i*bs + j].y  = particles[i].y[j];
      ref[i*bs + j].z  = particles[i].z[j];
      ref[i*bs + j].vx = particles[i].vx[j];
      ref[i*bs + j].vy = particles[i].vy[j];
      ref[i*bs + j].vz = particles[i].vz[j];
      ref[i*bs + j].m  = particles[i].m[j];
      ref[i*bs + j].Gm = particles[i].Gm[j];
    }
  }

  char fn[1024];
  sprintf(fn, "nbody-%d-%d.out", num_particles, timesteps);

  const int fd = open(fn, O_CREAT | O_WRONLY, S_IWUSR | S_IRUSR);
  //  assert(fd >= 0);

  write(fd, ref, num_particles * sizeof(particle_t));
  close(fd);
}

int main(int argc, char** argv)
{
  const int num_particles = 512;
  const int timesteps = 1;

//  soa_particle_t * QUAL particles = (soa_particle_t*)malloc(num_particles/bs * sizeof(soa_particle_t));
//  soa_force_t * QUAL forces = (soa_force_t*)malloc(num_particles/bs * sizeof(soa_force_t));

    soa_particle_t * QUAL particles;
    soa_force_t * QUAL forces;

    if (posix_memalign((void **)&particles, 64, num_particles/bs * sizeof(soa_particle_t)))
    {
        fprintf(stderr, "ERROR allocating 'a'\n");
        return 1;
    }
    if (posix_memalign((void **)&forces, 64, num_particles/bs * sizeof(soa_force_t)))
    {
        fprintf(stderr, "ERROR allocating 'a'\n");
        return 1;
    }


  int i;

  init_particles(particles, num_particles);
  init_forces(forces, num_particles);

  for (i = 0; i < timesteps; ++i) {
    calculate_nbody(particles, forces, num_particles);
  }

  //save_results(particles, num_particles, timesteps);
  //check_results(particles, num_particles, timesteps);

  //  free(particles);
  //  free(forces);

  return 0;
}
