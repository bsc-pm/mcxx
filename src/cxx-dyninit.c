#include "cxx-dyninit.h"
#include "cxx-utils.h"

static dynamic_initializer_t* dynamic_initializer_list = NULL;
static int already_initialized = 0;
static int num_dynamic_initializers = 0;

void register_dynamic_initializer(dynamic_initializer_t dynamic_initializer)
{
    P_LIST_ADD(dynamic_initializer_list, num_dynamic_initializers, dynamic_initializer);
}

void run_dynamic_initializers(void)
{
    if (already_initialized)
        return;

    int i;
    for (i = 0; i < num_dynamic_initializers; i++)
    {
        (dynamic_initializer_list[i])();
    }

    already_initialized = 1;
}
