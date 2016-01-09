/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion

  This file is part of Mercurium C/C++ source-to-source compiler.

  See AUTHORS file in the top level directory for information
  regarding developers and contributors.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.

  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.

  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/



/*
<testinfo>
test_generator=config/mercurium-analysis
test_nolink=yes
</testinfo>
*/

typedef struct caCmBordersS
{
    uint32_t left_x;
    uint32_t right_x;
    uint32_t left_y;
    uint32_t right_y;
    uint32_t left_z;
    uint32_t right_z;
} caCmBorders;

typedef struct caCmIpBordersS
{
    uint32_t left_x;
    uint32_t right_x;
    uint32_t left_y;
    uint32_t right_y;
    uint32_t left_z;
    uint32_t right_z;
} caCmIpBorders;

typedef struct caCmCompartmentS
{
    uint32_t m_num; //indexes for presentation
    uint32_t m_x;
    uint32_t m_y;
    uint32_t m_z;
    uint32_t m_thdnum; //num of all acting compartments
    
    //borders for gridsInit
    caCmBorders m_brd1;
    caCmBorders m_brd2;
    caCmBorders m_brd4;
    
    //interpolation borders for gridInterpolationUp and gridInterpolationDown
    caCmIpBorders m_ip_brd1;
    caCmIpBorders m_ip_brd2;
    caCmIpBorders m_ip_brd4;
    
} caCmCompartment;

const int X_COMPARTMENT_NUM = 4;
const int Y_COMPARTMENT_NUM = 4;
const int Z_COMPARTMENT_NUM = 1;

caCmCompartment *m_compartments[X_COMPARTMENT_NUM][Y_COMPARTMENT_NUM][Z_COMPARTMENT_NUM];

void prepare_ph(caCmCompartment *c, int32_t step);
void iteration_ph(caCmCompartment *c, int32_t step);
void complete_ph(caCmCompartment *c, int32_t step);

void compartmentsComputation (void)
{
    int32_t x, y, z;
    int32_t step;
    int32_t done_state;

    #pragma analysis_check assert range(step:1:6:0)
    for (step=1; step <= 5; step++) 
    {
        #pragma analysis_check assert range(step:1:5:0; x:0:4:0)
        for (x = 0; x < X_COMPARTMENT_NUM; x++) {
            #pragma analysis_check assert range(step:1:5:0; x:0:3:0; y:0:4:0)
            for (y = 0; y < Y_COMPARTMENT_NUM; y++) {
                #pragma analysis_check assert \
                        range(step:1:5:0; x:0:3:0; y:0:3:0; z:0:1:0)
                for (z = 0; z < Z_COMPARTMENT_NUM; z++) {
                    #pragma analysis_check assert range(step:1:5:0; x:0:3:0; y:0:3:0; z:0:0:0)
                    #pragma omp task firstprivate(step) \
                            depend(inout:m_compartments[x][y][z])
                    prepare_ph(m_compartments[x][y][z], step);
                }
            }
        }

        #pragma analysis_check assert range(step:1:5:0; done_state:1:13:0)
        for (done_state=1; done_state <= 12; done_state++) {
            #pragma analysis_check assert \
                    range(step:1:5:0; done_state:1:12:0; x:0:4:0)
            #pragma analysis_check assert range(x:0:4:0)
            for (x = 0; x < X_COMPARTMENT_NUM; x++) {
                #pragma analysis_check assert \
                        range(step:1:5:0; done_state:1:12:0; x:0:3:0; y:0:4:0)
                for (y = 0; y < Y_COMPARTMENT_NUM; y++) {
                    #pragma analysis_check assert \
                            range(step:1:5:0; done_state:1:12:0; x:0:3:0; y:0:3:0; z:0:1:0)
                    for (z = 0; z < Z_COMPARTMENT_NUM; z++) {

                        // blocks in upper left corner of the cube  
                        if (x == 0 && y == 0 && z == 0) {                           // 1
                            #pragma analysis_check assert range(x:0:0:0; y:0:0:0; z:0:0:0)
                            #pragma omp task firstprivate(step) \
                                    depend(in:m_compartments[x+1][y][z]) \
                                    depend(in:m_compartments[x][y+1][z]) \
                                    depend(inout:m_compartments[x][y][z])
                            iteration_ph(m_compartments[x][y][z], step);
                        }
                        // block in left lower corner of the cube
                        else if (x == 0 && z == 0 && (y == Y_COMPARTMENT_NUM-1)) {  // 3
                            #pragma analysis_check assert range(x:0:0:0; y:3:3:0; z:0:0:0)
                            #pragma omp task firstprivate(step) \
                                    depend(in: m_compartments[x][y-1][z]) \
                                    depend(in: m_compartments[x+1][y][z]) \
                                    depend(inout:m_compartments[x][y][z])
                            iteration_ph(m_compartments[x][y][z], step);
                        }
                        // blocks in left edge of the cube
                        else if (x == 0 && z == 0 && (y != Y_COMPARTMENT_NUM-1)) {  // 2
                            #pragma analysis_check assert range(x:0:0:0; y:0:2:0; z:0:0:0)
                            #pragma omp task firstprivate(step) \
                                    depend(in: m_compartments[x][y-1][z]) \
                                    depend(in: m_compartments[x][y+1][z]) \
                                    depend(in: m_compartments[x+1][y][z]) \
                                    depend(inout:m_compartments[x][y][z])
                            iteration_ph(m_compartments[x][y][z], step);
                        } 
                        // blocks in upper edge of the cube
                        else if (y == 0 && z == 0 && (x != X_COMPARTMENT_NUM-1)) {  // 4
                            #pragma analysis_check assert range(x:0:2:0; y:0:0:0; z:0:0:0)
                            #pragma omp task firstprivate(step) \
                                    depend(in: m_compartments[x-1][y][z]) \
                                    depend(in: m_compartments[x][y+1][z]) \
                                    depend(in: m_compartments[x+1][y][z]) \
                                    depend(inout:m_compartments[x][y][z])
                            iteration_ph(m_compartments[x][y][z], step);
                        } 
                        // blocks in upper right corner of the cube 
                        else if (y == 0 && z == 0 && (x == X_COMPARTMENT_NUM-1)) {  // 7
                            #pragma analysis_check assert range(x:3:3:0; y:0:0:0; z:0:0:0)
                            #pragma omp task firstprivate(step) \
                                    depend(in: m_compartments[x-1][y][z]) \
                                    depend(in: m_compartments[x][y+1][z]) \
                                    depend(inout:m_compartments[x][y][z])
                                iteration_ph(m_compartments[x][y][z], step);
                        } 
                        // blocks in right edge of the cube
                        else if ((x == X_COMPARTMENT_NUM-1)  && (y != Y_COMPARTMENT_NUM-1) && z == 0) {  // 8 
                            #pragma analysis_check assert range(x:3:3:0; y:0:2:0; z:0:0:0)
                            #pragma omp task firstprivate(step) \
                                    depend(in: m_compartments[x][y-1][z]) \
                                    depend(in: m_compartments[x][y+1][z]) \
                                    depend(in: m_compartments[x-1][y][z]) \
                                    depend(inout:m_compartments[x][y][z])
                            iteration_ph(m_compartments[x][y][z], step);
                        } 
                        // block in right lower corner of the cube
                        else if ((x == X_COMPARTMENT_NUM-1)  && (y == Y_COMPARTMENT_NUM-1) && z == 0) {   // 9
                            #pragma analysis_check assert range(x:3:3:0; y:3:3:0; z:0:0:0)
                            #pragma omp task firstprivate(step) \
                                    depend(in: m_compartments[x][y-1][z]) \
                                    depend(in: m_compartments[x-1][y][z]) \
                                    depend(inout:m_compartments[x][y][z])
                                iteration_ph(m_compartments[x][y][z], step);
                        } 
                        // block in right lower corner of the cube
                        else if ((x != X_COMPARTMENT_NUM-1)  && (y == Y_COMPARTMENT_NUM-1) && z == 0) {  // 6 
                            #pragma analysis_check assert range(x:0:2:0; y:3:3:0; z:0:0:0)
                            #pragma omp task firstprivate(step) \
                                    depend(in: m_compartments[x][y-1][z]) \
                                    depend(in: m_compartments[x+1][y][z]) \
                                    depend(in: m_compartments[x-1][y][z]) \
                                    depend(inout:m_compartments[x][y][z])
                                iteration_ph(m_compartments[x][y][z], step);
                        } 
                        // Internal blocks
                        else {                          // 5
                            // NOTE: Range Analysis does not manage accurately && and ||
                            //       So here we may have any possible range for x and y
                            #pragma analysis_check assert range(x:0:3:0; y:0:3:0; z:0:0:0)
                            #pragma omp task firstprivate(step) \
                                    depend(in: m_compartments[x-1][y][z]) \
                                    depend(in: m_compartments[x+1][y][z]) \
                                    depend(in: m_compartments[x][y-1][z]) \
                                    depend(in: m_compartments[x][y+1][z]) \
                                    depend(inout:m_compartments[x][y][z])
                                iteration_ph(m_compartments[x][y][z], step);
                        }    
                    }
                }
            }
        }

        #pragma analysis_check assert range(step:1:5:0; x:0:4:0)
        for(x = 0; x < X_COMPARTMENT_NUM; x++) {
            #pragma analysis_check assert range(step:1:5:0; x:0:3:0; y:0:4:0)
            for(y = 0; y < Y_COMPARTMENT_NUM; y++) {
                #pragma analysis_check assert range(step:1:5:0; x:0:3:0; y:0:3:0; z:0:1:0)
                for(z = 0; z < Z_COMPARTMENT_NUM; z++) {
                    #pragma analysis_check assert range(step:1:5:0; x:0:3:0; y:0:3:0; z:0:0:0)
                    #pragma omp task firstprivate(step) \
                            depend(inout:m_compartments[x][y][z])
                    complete_ph(m_compartments[x][y][z], step);
                }
            }
        }

        #pragma omp taskwait
    }
}
