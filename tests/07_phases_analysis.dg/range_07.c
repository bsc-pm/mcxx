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
#define UINT8BIT unsigned char
#define UINT16BIT unsigned short int
#define UINT32BIT unsigned int
#define INT16BIT short int
#define INT32BIT int

#define FRAME_DIM_X 2048
#define FRAME_DIM_Y 2048
const int BS = 512;
#define DIM_X (FRAME_DIM_X/BS)
#define DIM_Y (FRAME_DIM_Y/BS)
#define NUMBER_OF_GROUPS_PER_EXPOSURE 5
#define NUMBER_OF_FRAMES_PER_GROUP 1
#define STRIPES_PER_FRAME 32

void detectSaturation(UINT16BIT frame[DIM_Y][DIM_X][BS][BS], 
                      UINT16BIT saturationLimit[STRIPES_PER_FRAME], 
                      UINT32BIT saturationFrame[DIM_Y][DIM_X][BS][BS/32], 
                      int i, int j);
void subtractSuperBias(UINT16BIT frame[DIM_Y][DIM_X][BS][BS], 
                       UINT16BIT bias[DIM_Y][DIM_X][BS][BS], 
                       int i, int j);
void nonLinearityCorrectionPolynomial(UINT16BIT frame[DIM_Y][DIM_X][BS][BS], 
                                      INT16BIT coeff[STRIPES_PER_FRAME][4], 
                                      int i, int j);
void subtractReferencePixelTopBottom(UINT16BIT frame[DIM_Y][DIM_X][BS][BS], int j);
void subtractReferencePixelSides(UINT16BIT frame[DIM_Y][DIM_X][BS][BS]);
void detectCosmicRay(UINT16BIT frame[DIM_Y][DIM_X][BS][BS], 
                     UINT32BIT sumXYFrame[DIM_Y][DIM_X][BS][BS], 
                     UINT32BIT sumYFrame[DIM_Y][DIM_X][BS][BS], 
                     UINT16BIT offsetCosmicFrame[DIM_Y][DIM_X][BS][BS], 
                     UINT8BIT numberOfFramesAfterCosmicRay[DIM_Y][DIM_X][BS][BS], 
                     UINT32BIT Nnow, 
                     int i, int j);
void progressiveLinearLeastSquaresFit(UINT16BIT frame[DIM_Y][DIM_X][BS][BS], 
                                      UINT32BIT sumXYFrame[DIM_Y][DIM_X][BS][BS], 
                                      UINT32BIT sumYFrame[DIM_Y][DIM_X][BS][BS], 
                                      UINT16BIT offsetCosmicFrame[DIM_Y][DIM_X][BS][BS], 
                                      UINT32BIT saturationFrame[DIM_Y][DIM_X][BS][BS/32],
                                      UINT32BIT N, int i, int j);
void calculateFinalSignalFrame(UINT32BIT sumXYFrame[DIM_Y][DIM_X][BS][BS], 
                               UINT32BIT sumYFrame[DIM_Y][DIM_X][BS][BS],
                               UINT32BIT N, int i, int j);

typedef UINT16BIT (*p_block_type)[BS];
typedef UINT16BIT frame_type[DIM_Y][DIM_X][BS][BS];
static frame_type currentFrame1;
static frame_type currentFrame2;
static frame_type currentFrame3;
static frame_type currentFrame4;
static frame_type currentFrame5;
static frame_type* currentFrame[] = {
    &currentFrame1,
    &currentFrame2,
    &currentFrame3,
    &currentFrame4,
    &currentFrame5
};

static UINT16BIT biasFrame[DIM_Y][DIM_X][BS][BS];
static UINT16BIT saturationLimit[STRIPES_PER_FRAME];
static UINT32BIT saturationFrame[DIM_Y][DIM_X][BS][BS/32];
static INT16BIT coeffOfNonLinearityPolynomial[STRIPES_PER_FRAME][4];
static UINT16BIT offsetCosmicFrame[DIM_Y][DIM_X][BS][BS];
static UINT8BIT numberOfFramesAfterCosmicRay[DIM_Y][DIM_X][BS][BS];
static UINT32BIT sumXYFrame[DIM_Y][DIM_X][BS][BS];
static UINT32BIT sumYFrame[DIM_Y][DIM_X][BS][BS];

int main (int argc, char *argv[])
{
    #pragma omp task
    {
        INT32BIT groupNumber;
        for (groupNumber=1; groupNumber<=NUMBER_OF_GROUPS_PER_EXPOSURE; groupNumber++) {
        }
    }
    #pragma omp taskwait

#pragma omp parallel
#pragma omp single
    {
        INT32BIT groupNumber=1;
        int i=0, j=0;
        #pragma analysis_check assert range(groupNumber:1:6:0)
        for (groupNumber=1; groupNumber<=NUMBER_OF_GROUPS_PER_EXPOSURE; groupNumber++) 
        {
            #pragma omp taskwait

            #pragma analysis_check assert range(i:0:4:0)
            for (i=0; i < DIM_Y; i++) {
                #pragma analysis_check assert range(i:0:3:0; j:0:4:0)
                for (j=0; j < DIM_X; j++) {
                    p_block_type p = (*currentFrame[groupNumber-1])[i][j];
                    #pragma omp task depend (in: p[0:BS], saturationLimit) \
                                     depend (inout: saturationFrame[i][j])
                    detectSaturation(*currentFrame[groupNumber-1], saturationLimit, saturationFrame, i, j);
                }
            }

            #pragma analysis_check assert range(i:0:4:0)
            for (i=0; i < DIM_Y; i++) {
                #pragma analysis_check assert range(i:0:3:0; j:0:4:0)
                for (j=0; j < DIM_X; j++) {
                    p_block_type p = (*currentFrame[groupNumber-1])[i][j];
                    #pragma omp task depend (in: biasFrame[i][j]) \
                                     depend (inout: p[0:BS])
                    subtractSuperBias(*currentFrame[groupNumber-1], biasFrame, i, j);
                }
            }

            #pragma analysis_check assert range(i:0:4:0)
            for (i=0; i < DIM_Y; i++) {
                #pragma analysis_check assert range(i:0:3:0; j:0:4:0)
                for (j=0; j < DIM_X; j++) {
                    p_block_type p = (*currentFrame[groupNumber-1])[i][j];
                    #pragma omp task depend (in: coeffOfNonLinearityPolynomial) \
                                     depend (inout: p[0:BS])
                    nonLinearityCorrectionPolynomial(*currentFrame[groupNumber-1], coeffOfNonLinearityPolynomial, i, j);
                }
            }

            #pragma omp taskwait

            #pragma analysis_check assert range(j:0:4:0)
            for (j=0; j < DIM_X; j++) {
                #pragma omp task firstprivate(j)
                subtractReferencePixelTopBottom(*currentFrame[groupNumber-1], j);
            }

            #pragma omp taskwait
            #pragma omp task
                subtractReferencePixelSides(*currentFrame[groupNumber-1]);
            #pragma omp taskwait

            #pragma analysis_check assert range(i:0:4:0)
            for (i=0; i < DIM_Y; i++) {
                #pragma analysis_check assert range(i:0:3:0; j:0:4:0)
                for (j=0; j < DIM_X; j++) {
                    p_block_type p = (*currentFrame[groupNumber-1])[i][j];
                    #pragma omp task firstprivate(groupNumber) \
                        depend (in: p[0:BS], sumXYFrame[i][j], sumYFrame[i][j]) \
                        depend (inout: offsetCosmicFrame[i][j], numberOfFramesAfterCosmicRay[i][j])
                    detectCosmicRay(*currentFrame[groupNumber-1], sumXYFrame, sumYFrame, offsetCosmicFrame, 
                            numberOfFramesAfterCosmicRay, groupNumber, i, j);
                }
            }

            #pragma analysis_check assert range(i:0:4:0)
            for (i=0; i < DIM_Y; i++) {
                #pragma analysis_check assert range(i:0:3:0; j:0:4:0)
                for (j=0; j < DIM_X; j++) {
                    p_block_type p = (*currentFrame[groupNumber-1])[i][j];
                    #pragma omp task firstprivate(groupNumber) \
                        depend (in: p[0:BS], offsetCosmicFrame[i][j], saturationFrame[i][j]) \
                        depend (inout: sumXYFrame[i][j], sumYFrame[i][j])
                    progressiveLinearLeastSquaresFit(*currentFrame[groupNumber-1], sumXYFrame, sumYFrame, 
                            offsetCosmicFrame, saturationFrame, groupNumber, i, j);
                }
            }
        }

        #pragma analysis_check assert range(i:0:4:0)
        for (i=0; i < DIM_Y; i++) {
            #pragma analysis_check assert range(i:0:3:0; j:0:4:0)
            for (j=0; j < DIM_X; j++) {
                #pragma omp task depend (in: sumXYFrame[i][j]) \
                                depend (inout: sumYFrame[i][j])
                calculateFinalSignalFrame(sumXYFrame, sumYFrame, NUMBER_OF_GROUPS_PER_EXPOSURE, i, j);
            }
        }
    }   // end of parallel + single

    return 0;
}
