/*
<testinfo>
test_generator="config/mercurium-hlt run"
</testinfo>
*/

#include <cassert>
#include <iostream>
#include <vector>

//------------
// Parameters
//------------

#define INIT_3 2
#define INIT_2 2
#define INIT_1 2

#define END_3 11
#define END_2 11
#define END_1 11

#define STEP_3 3
#define STEP_2 5
#define STEP_1 5

//---------
// Defines
//---------

#define MAX(a, b) ((a) > (b)? (a):(b))

// Infinite loop detection

#if (STEP_1 == 0 || STEP_2 == 0 || STEP_3 == 0)
    #error Infinite loop detected, step is zero
#endif

int main()
{
    int expected_3;
    int expected_2;
    int expected_1;
    int result;

    // ########### Direct loop (init <= end) ###########

    // -----------  <  --  += +  -----------
    {
        // Direct condition
        #if \
            ((INIT_1 < END_1) && ((STEP_1) < 0)) || \
            ((INIT_2 < END_2) && ((STEP_2) < 0)) || \
            ((INIT_3 < END_3) && ((STEP_3) < 0))
            #error Infinite loop detected (direct)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(INIT_3, END_3) + 1,
                std::vector< std::vector<int> >(MAX(INIT_2, END_2) + 1,
                    std::vector<int>(MAX(INIT_1, END_1) + 1, 8)));

        for (int i = INIT_3; i < END_3; i += STEP_3)
        for (int j = INIT_2; j < END_2; j += STEP_2)
        for (int k = INIT_1; k < END_1; k += STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = INIT_3; i < END_3; i += STEP_3)
        for (int j = INIT_2; j < END_2; j += STEP_2)
        for (int k = INIT_1; k < END_1; k += STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = INIT_3; i < END_3; i += STEP_3)
        for (int j = INIT_2; j < END_2; j += STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = INIT_3; i < END_3; i += STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = INIT_3; i < END_3; i += STEP_3)
        for (int j = INIT_2; j < END_2; j += STEP_2)
        for (int k = INIT_1; k < END_1; k += STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = INIT_3; i < END_3; i += STEP_3)
        for (int j = INIT_2; j < END_2; j += STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = INIT_3; i < END_3; i += STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  <  --  -= -  -----------
    {
        // Reverse condition
        #if \
            ((END_1 < INIT_1) && !((-STEP_1) < 0)) || \
            ((END_2 < INIT_2) && !((-STEP_2) < 0)) || \
            ((END_3 < INIT_3) && !((-STEP_3) < 0))
            #error Infinite loop detected (reverse)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(INIT_3, END_3) + 1,
                std::vector< std::vector<int> >(MAX(INIT_2, END_2) + 1,
                    std::vector<int>(MAX(INIT_1, END_1) + 1, 8)));

        for (int i = INIT_3; i < END_3; i -= -STEP_3)
        for (int j = INIT_2; j < END_2; j -= -STEP_2)
        for (int k = INIT_1; k < END_1; k -= -STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = INIT_3; i < END_3; i -= -STEP_3)
        for (int j = INIT_2; j < END_2; j -= -STEP_2)
        for (int k = INIT_1; k < END_1; k -= -STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = INIT_3; i < END_3; i -= -STEP_3)
        for (int j = INIT_2; j < END_2; j -= -STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = INIT_3; i < END_3; i -= -STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = INIT_3; i < END_3; i -= -STEP_3)
        for (int j = INIT_2; j < END_2; j -= -STEP_2)
        for (int k = INIT_1; k < END_1; k -= -STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = INIT_3; i < END_3; i -= -STEP_3)
        for (int j = INIT_2; j < END_2; j -= -STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = INIT_3; i < END_3; i -= -STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  <=  --  += +  -----------
    {
        // Direct condition
        #if \
            ((INIT_1 <= END_1) && ((STEP_1) <= 0)) || \
            ((INIT_2 <= END_2) && ((STEP_2) <= 0)) || \
            ((INIT_3 <= END_3) && ((STEP_3) <= 0))
            #error Infinite loop detected (direct)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(INIT_3, END_3) + 1,
                std::vector< std::vector<int> >(MAX(INIT_2, END_2) + 1,
                    std::vector<int>(MAX(INIT_1, END_1) + 1, 8)));

        for (int i = INIT_3; i <= END_3; i += STEP_3)
        for (int j = INIT_2; j <= END_2; j += STEP_2)
        for (int k = INIT_1; k <= END_1; k += STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = INIT_3; i <= END_3; i += STEP_3)
        for (int j = INIT_2; j <= END_2; j += STEP_2)
        for (int k = INIT_1; k <= END_1; k += STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = INIT_3; i <= END_3; i += STEP_3)
        for (int j = INIT_2; j <= END_2; j += STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = INIT_3; i <= END_3; i += STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = INIT_3; i <= END_3; i += STEP_3)
        for (int j = INIT_2; j <= END_2; j += STEP_2)
        for (int k = INIT_1; k <= END_1; k += STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = INIT_3; i <= END_3; i += STEP_3)
        for (int j = INIT_2; j <= END_2; j += STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = INIT_3; i <= END_3; i += STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  <=  --  -= -  -----------
    {
        // Reverse condition
        #if \
            ((END_1 <= INIT_1) && !((-STEP_1) <= 0)) || \
            ((END_2 <= INIT_2) && !((-STEP_2) <= 0)) || \
            ((END_3 <= INIT_3) && !((-STEP_3) <= 0))
            #error Infinite loop detected (reverse)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(INIT_3, END_3) + 1,
                std::vector< std::vector<int> >(MAX(INIT_2, END_2) + 1,
                    std::vector<int>(MAX(INIT_1, END_1) + 1, 8)));

        for (int i = INIT_3; i <= END_3; i -= -STEP_3)
        for (int j = INIT_2; j <= END_2; j -= -STEP_2)
        for (int k = INIT_1; k <= END_1; k -= -STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = INIT_3; i <= END_3; i -= -STEP_3)
        for (int j = INIT_2; j <= END_2; j -= -STEP_2)
        for (int k = INIT_1; k <= END_1; k -= -STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = INIT_3; i <= END_3; i -= -STEP_3)
        for (int j = INIT_2; j <= END_2; j -= -STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = INIT_3; i <= END_3; i -= -STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = INIT_3; i <= END_3; i -= -STEP_3)
        for (int j = INIT_2; j <= END_2; j -= -STEP_2)
        for (int k = INIT_1; k <= END_1; k -= -STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = INIT_3; i <= END_3; i -= -STEP_3)
        for (int j = INIT_2; j <= END_2; j -= -STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = INIT_3; i <= END_3; i -= -STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  >  --  += -  -----------
    {
        // Direct condition
        #if \
            ((INIT_1 > END_1) && ((-STEP_1) > 0)) || \
            ((INIT_2 > END_2) && ((-STEP_2) > 0)) || \
            ((INIT_3 > END_3) && ((-STEP_3) > 0))
            #error Infinite loop detected (direct)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(INIT_3, END_3) + 1,
                std::vector< std::vector<int> >(MAX(INIT_2, END_2) + 1,
                    std::vector<int>(MAX(INIT_1, END_1) + 1, 8)));

        for (int i = INIT_3; i > END_3; i += -STEP_3)
        for (int j = INIT_2; j > END_2; j += -STEP_2)
        for (int k = INIT_1; k > END_1; k += -STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = INIT_3; i > END_3; i += -STEP_3)
        for (int j = INIT_2; j > END_2; j += -STEP_2)
        for (int k = INIT_1; k > END_1; k += -STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = INIT_3; i > END_3; i += -STEP_3)
        for (int j = INIT_2; j > END_2; j += -STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = INIT_3; i > END_3; i += -STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = INIT_3; i > END_3; i += -STEP_3)
        for (int j = INIT_2; j > END_2; j += -STEP_2)
        for (int k = INIT_1; k > END_1; k += -STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = INIT_3; i > END_3; i += -STEP_3)
        for (int j = INIT_2; j > END_2; j += -STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = INIT_3; i > END_3; i += -STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  >  --  -= +  -----------
    {
        // Reverse condition
        #if \
            ((END_1 > INIT_1) && !((STEP_1) > 0)) || \
            ((END_2 > INIT_2) && !((STEP_2) > 0)) || \
            ((END_3 > INIT_3) && !((STEP_3) > 0))
            #error Infinite loop detected (reverse)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(INIT_3, END_3) + 1,
                std::vector< std::vector<int> >(MAX(INIT_2, END_2) + 1,
                    std::vector<int>(MAX(INIT_1, END_1) + 1, 8)));

        for (int i = INIT_3; i > END_3; i -= STEP_3)
        for (int j = INIT_2; j > END_2; j -= STEP_2)
        for (int k = INIT_1; k > END_1; k -= STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = INIT_3; i > END_3; i -= STEP_3)
        for (int j = INIT_2; j > END_2; j -= STEP_2)
        for (int k = INIT_1; k > END_1; k -= STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = INIT_3; i > END_3; i -= STEP_3)
        for (int j = INIT_2; j > END_2; j -= STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = INIT_3; i > END_3; i -= STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = INIT_3; i > END_3; i -= STEP_3)
        for (int j = INIT_2; j > END_2; j -= STEP_2)
        for (int k = INIT_1; k > END_1; k -= STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = INIT_3; i > END_3; i -= STEP_3)
        for (int j = INIT_2; j > END_2; j -= STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = INIT_3; i > END_3; i -= STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  >=  --  += -  -----------
    {
        // Direct condition
        #if \
            ((INIT_1 >= END_1) && ((-STEP_1) >= 0)) || \
            ((INIT_2 >= END_2) && ((-STEP_2) >= 0)) || \
            ((INIT_3 >= END_3) && ((-STEP_3) >= 0))
            #error Infinite loop detected (direct)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(INIT_3, END_3) + 1,
                std::vector< std::vector<int> >(MAX(INIT_2, END_2) + 1,
                    std::vector<int>(MAX(INIT_1, END_1) + 1, 8)));

        for (int i = INIT_3; i >= END_3; i += -STEP_3)
        for (int j = INIT_2; j >= END_2; j += -STEP_2)
        for (int k = INIT_1; k >= END_1; k += -STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = INIT_3; i >= END_3; i += -STEP_3)
        for (int j = INIT_2; j >= END_2; j += -STEP_2)
        for (int k = INIT_1; k >= END_1; k += -STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = INIT_3; i >= END_3; i += -STEP_3)
        for (int j = INIT_2; j >= END_2; j += -STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = INIT_3; i >= END_3; i += -STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = INIT_3; i >= END_3; i += -STEP_3)
        for (int j = INIT_2; j >= END_2; j += -STEP_2)
        for (int k = INIT_1; k >= END_1; k += -STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = INIT_3; i >= END_3; i += -STEP_3)
        for (int j = INIT_2; j >= END_2; j += -STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = INIT_3; i >= END_3; i += -STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  >=  --  -= +  -----------
    {
        // Reverse condition
        #if \
            ((END_1 >= INIT_1) && !((STEP_1) >= 0)) || \
            ((END_2 >= INIT_2) && !((STEP_2) >= 0)) || \
            ((END_3 >= INIT_3) && !((STEP_3) >= 0))
            #error Infinite loop detected (reverse)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(INIT_3, END_3) + 1,
                std::vector< std::vector<int> >(MAX(INIT_2, END_2) + 1,
                    std::vector<int>(MAX(INIT_1, END_1) + 1, 8)));

        for (int i = INIT_3; i >= END_3; i -= STEP_3)
        for (int j = INIT_2; j >= END_2; j -= STEP_2)
        for (int k = INIT_1; k >= END_1; k -= STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = INIT_3; i >= END_3; i -= STEP_3)
        for (int j = INIT_2; j >= END_2; j -= STEP_2)
        for (int k = INIT_1; k >= END_1; k -= STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = INIT_3; i >= END_3; i -= STEP_3)
        for (int j = INIT_2; j >= END_2; j -= STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = INIT_3; i >= END_3; i -= STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = INIT_3; i >= END_3; i -= STEP_3)
        for (int j = INIT_2; j >= END_2; j -= STEP_2)
        for (int k = INIT_1; k >= END_1; k -= STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = INIT_3; i >= END_3; i -= STEP_3)
        for (int j = INIT_2; j >= END_2; j -= STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = INIT_3; i >= END_3; i -= STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }

    // ########### Reverse loop (init >= end) ###########

    // -----------  <  --  += +  -----------
    {
        // Direct condition
        #if \
            ((INIT_1 < END_1) && ((STEP_1) < 0)) || \
            ((INIT_2 < END_2) && ((STEP_2) < 0)) || \
            ((INIT_3 < END_3) && ((STEP_3) < 0))
            #error Infinite loop detected (direct)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(END_3, INIT_3) + 1,
                std::vector< std::vector<int> >(MAX(END_2, INIT_2) + 1,
                    std::vector<int>(MAX(END_1, INIT_1) + 1, 8)));

        for (int i = END_3; i < INIT_3; i += STEP_3)
        for (int j = END_2; j < INIT_2; j += STEP_2)
        for (int k = END_1; k < INIT_1; k += STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = END_3; i < INIT_3; i += STEP_3)
        for (int j = END_2; j < INIT_2; j += STEP_2)
        for (int k = END_1; k < INIT_1; k += STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = END_3; i < INIT_3; i += STEP_3)
        for (int j = END_2; j < INIT_2; j += STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = END_3; i < INIT_3; i += STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = END_3; i < INIT_3; i += STEP_3)
        for (int j = END_2; j < INIT_2; j += STEP_2)
        for (int k = END_1; k < INIT_1; k += STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = END_3; i < INIT_3; i += STEP_3)
        for (int j = END_2; j < INIT_2; j += STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = END_3; i < INIT_3; i += STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  <  --  -= -  -----------
    {
        // Reverse condition
        #if \
            ((END_1 < INIT_1) && !((-STEP_1) < 0)) || \
            ((END_2 < INIT_2) && !((-STEP_2) < 0)) || \
            ((END_3 < INIT_3) && !((-STEP_3) < 0))
            #error Infinite loop detected (reverse)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(END_3, INIT_3) + 1,
                std::vector< std::vector<int> >(MAX(END_2, INIT_2) + 1,
                    std::vector<int>(MAX(END_1, INIT_1) + 1, 8)));

        for (int i = END_3; i < INIT_3; i -= -STEP_3)
        for (int j = END_2; j < INIT_2; j -= -STEP_2)
        for (int k = END_1; k < INIT_1; k -= -STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = END_3; i < INIT_3; i -= -STEP_3)
        for (int j = END_2; j < INIT_2; j -= -STEP_2)
        for (int k = END_1; k < INIT_1; k -= -STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = END_3; i < INIT_3; i -= -STEP_3)
        for (int j = END_2; j < INIT_2; j -= -STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = END_3; i < INIT_3; i -= -STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = END_3; i < INIT_3; i -= -STEP_3)
        for (int j = END_2; j < INIT_2; j -= -STEP_2)
        for (int k = END_1; k < INIT_1; k -= -STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = END_3; i < INIT_3; i -= -STEP_3)
        for (int j = END_2; j < INIT_2; j -= -STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = END_3; i < INIT_3; i -= -STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  <=  --  += +  -----------
    {
        // Direct condition
        #if \
            ((INIT_1 <= END_1) && ((STEP_1) <= 0)) || \
            ((INIT_2 <= END_2) && ((STEP_2) <= 0)) || \
            ((INIT_3 <= END_3) && ((STEP_3) <= 0))
            #error Infinite loop detected (direct)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(END_3, INIT_3) + 1,
                std::vector< std::vector<int> >(MAX(END_2, INIT_2) + 1,
                    std::vector<int>(MAX(END_1, INIT_1) + 1, 8)));

        for (int i = END_3; i <= INIT_3; i += STEP_3)
        for (int j = END_2; j <= INIT_2; j += STEP_2)
        for (int k = END_1; k <= INIT_1; k += STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = END_3; i <= INIT_3; i += STEP_3)
        for (int j = END_2; j <= INIT_2; j += STEP_2)
        for (int k = END_1; k <= INIT_1; k += STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = END_3; i <= INIT_3; i += STEP_3)
        for (int j = END_2; j <= INIT_2; j += STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = END_3; i <= INIT_3; i += STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = END_3; i <= INIT_3; i += STEP_3)
        for (int j = END_2; j <= INIT_2; j += STEP_2)
        for (int k = END_1; k <= INIT_1; k += STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = END_3; i <= INIT_3; i += STEP_3)
        for (int j = END_2; j <= INIT_2; j += STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = END_3; i <= INIT_3; i += STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  <=  --  -= -  -----------
    {
        // Reverse condition
        #if \
            ((END_1 <= INIT_1) && !((-STEP_1) <= 0)) || \
            ((END_2 <= INIT_2) && !((-STEP_2) <= 0)) || \
            ((END_3 <= INIT_3) && !((-STEP_3) <= 0))
            #error Infinite loop detected (reverse)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(END_3, INIT_3) + 1,
                std::vector< std::vector<int> >(MAX(END_2, INIT_2) + 1,
                    std::vector<int>(MAX(END_1, INIT_1) + 1, 8)));

        for (int i = END_3; i <= INIT_3; i -= -STEP_3)
        for (int j = END_2; j <= INIT_2; j -= -STEP_2)
        for (int k = END_1; k <= INIT_1; k -= -STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = END_3; i <= INIT_3; i -= -STEP_3)
        for (int j = END_2; j <= INIT_2; j -= -STEP_2)
        for (int k = END_1; k <= INIT_1; k -= -STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = END_3; i <= INIT_3; i -= -STEP_3)
        for (int j = END_2; j <= INIT_2; j -= -STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = END_3; i <= INIT_3; i -= -STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = END_3; i <= INIT_3; i -= -STEP_3)
        for (int j = END_2; j <= INIT_2; j -= -STEP_2)
        for (int k = END_1; k <= INIT_1; k -= -STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = END_3; i <= INIT_3; i -= -STEP_3)
        for (int j = END_2; j <= INIT_2; j -= -STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = END_3; i <= INIT_3; i -= -STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  >  --  += -  -----------
    {
        // Direct condition
        #if \
            ((INIT_1 > END_1) && ((-STEP_1) > 0)) || \
            ((INIT_2 > END_2) && ((-STEP_2) > 0)) || \
            ((INIT_3 > END_3) && ((-STEP_3) > 0))
            #error Infinite loop detected (direct)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(END_3, INIT_3) + 1,
                std::vector< std::vector<int> >(MAX(END_2, INIT_2) + 1,
                    std::vector<int>(MAX(END_1, INIT_1) + 1, 8)));

        for (int i = END_3; i > INIT_3; i += -STEP_3)
        for (int j = END_2; j > INIT_2; j += -STEP_2)
        for (int k = END_1; k > INIT_1; k += -STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = END_3; i > INIT_3; i += -STEP_3)
        for (int j = END_2; j > INIT_2; j += -STEP_2)
        for (int k = END_1; k > INIT_1; k += -STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = END_3; i > INIT_3; i += -STEP_3)
        for (int j = END_2; j > INIT_2; j += -STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = END_3; i > INIT_3; i += -STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = END_3; i > INIT_3; i += -STEP_3)
        for (int j = END_2; j > INIT_2; j += -STEP_2)
        for (int k = END_1; k > INIT_1; k += -STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = END_3; i > INIT_3; i += -STEP_3)
        for (int j = END_2; j > INIT_2; j += -STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = END_3; i > INIT_3; i += -STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  >  --  -= +  -----------
    {
        // Reverse condition
        #if \
            ((END_1 > INIT_1) && !((STEP_1) > 0)) || \
            ((END_2 > INIT_2) && !((STEP_2) > 0)) || \
            ((END_3 > INIT_3) && !((STEP_3) > 0))
            #error Infinite loop detected (reverse)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(END_3, INIT_3) + 1,
                std::vector< std::vector<int> >(MAX(END_2, INIT_2) + 1,
                    std::vector<int>(MAX(END_1, INIT_1) + 1, 8)));

        for (int i = END_3; i > INIT_3; i -= STEP_3)
        for (int j = END_2; j > INIT_2; j -= STEP_2)
        for (int k = END_1; k > INIT_1; k -= STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = END_3; i > INIT_3; i -= STEP_3)
        for (int j = END_2; j > INIT_2; j -= STEP_2)
        for (int k = END_1; k > INIT_1; k -= STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = END_3; i > INIT_3; i -= STEP_3)
        for (int j = END_2; j > INIT_2; j -= STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = END_3; i > INIT_3; i -= STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = END_3; i > INIT_3; i -= STEP_3)
        for (int j = END_2; j > INIT_2; j -= STEP_2)
        for (int k = END_1; k > INIT_1; k -= STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = END_3; i > INIT_3; i -= STEP_3)
        for (int j = END_2; j > INIT_2; j -= STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = END_3; i > INIT_3; i -= STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  >=  --  += -  -----------
    {
        // Direct condition
        #if \
            ((INIT_1 >= END_1) && ((-STEP_1) >= 0)) || \
            ((INIT_2 >= END_2) && ((-STEP_2) >= 0)) || \
            ((INIT_3 >= END_3) && ((-STEP_3) >= 0))
            #error Infinite loop detected (direct)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(END_3, INIT_3) + 1,
                std::vector< std::vector<int> >(MAX(END_2, INIT_2) + 1,
                    std::vector<int>(MAX(END_1, INIT_1) + 1, 8)));

        for (int i = END_3; i >= INIT_3; i += -STEP_3)
        for (int j = END_2; j >= INIT_2; j += -STEP_2)
        for (int k = END_1; k >= INIT_1; k += -STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = END_3; i >= INIT_3; i += -STEP_3)
        for (int j = END_2; j >= INIT_2; j += -STEP_2)
        for (int k = END_1; k >= INIT_1; k += -STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = END_3; i >= INIT_3; i += -STEP_3)
        for (int j = END_2; j >= INIT_2; j += -STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = END_3; i >= INIT_3; i += -STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = END_3; i >= INIT_3; i += -STEP_3)
        for (int j = END_2; j >= INIT_2; j += -STEP_2)
        for (int k = END_1; k >= INIT_1; k += -STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = END_3; i >= INIT_3; i += -STEP_3)
        for (int j = END_2; j >= INIT_2; j += -STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = END_3; i >= INIT_3; i += -STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
    // -----------  >=  --  -= +  -----------
    {
        // Reverse condition
        #if \
            ((END_1 >= INIT_1) && !((STEP_1) >= 0)) || \
            ((END_2 >= INIT_2) && !((STEP_2) >= 0)) || \
            ((END_3 >= INIT_3) && !((STEP_3) >= 0))
            #error Infinite loop detected (reverse)
        #endif

        // Initialization
        std::vector< std::vector< std::vector<int> > > M(
                MAX(END_3, INIT_3) + 1,
                std::vector< std::vector<int> >(MAX(END_2, INIT_2) + 1,
                    std::vector<int>(MAX(END_1, INIT_1) + 1, 8)));

        for (int i = END_3; i >= INIT_3; i -= STEP_3)
        for (int j = END_2; j >= INIT_2; j -= STEP_2)
        for (int k = END_1; k >= INIT_1; k -= STEP_1)
        {
            M[i][j][k] = i + j + k;
        }

        // Compute expected results

        expected_3 = expected_2 = expected_1 = 0;

        for (int i = END_3; i >= INIT_3; i -= STEP_3)
        for (int j = END_2; j >= INIT_2; j -= STEP_2)
        for (int k = END_1; k >= INIT_1; k -= STEP_1)
        {
            expected_3 += M[i][j][k]*2;
        }

        for (int i = END_3; i >= INIT_3; i -= STEP_3)
        for (int j = END_2; j >= INIT_2; j -= STEP_2)
        {
            expected_2 += M[i][j][0]*2;
        }

        for (int i = END_3; i >= INIT_3; i -= STEP_3)
        {
            expected_1 += M[i][0][0]*2;
        }

        // Collapsed loops

        result = 0;

        #pragma hlt collapse(3)
        for (int i = END_3; i >= INIT_3; i -= STEP_3)
        for (int j = END_2; j >= INIT_2; j -= STEP_2)
        for (int k = END_1; k >= INIT_1; k -= STEP_1)
        {
            result += M[i][j][k]*2;
        }

        assert(result == expected_3);

        result = 0;

        #pragma hlt collapse(2)
        for (int i = END_3; i >= INIT_3; i -= STEP_3)
        for (int j = END_2; j >= INIT_2; j -= STEP_2)
        {
            result += M[i][j][0]*2;
        }

        assert(result == expected_2);

        result = 0;

        #pragma hlt collapse(1)
        for (int i = END_3; i >= INIT_3; i -= STEP_3)
        {
            result += M[i][0][0]*2;
        }

        assert(result == expected_1);
    }
}
