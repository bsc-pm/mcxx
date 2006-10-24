#ifndef INI_H
#define INI_H

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <gc.h>

#ifdef __cplusplus
extern "C" {
#endif

#define     MS_STYLE            1
#define     NOT_MS_STYLE        2

/*
** function prototypes
*/
typedef
enum param_process_result_tag
{
    PPR_SUCCESS = 0,
    PPR_OPEN_FILE_ERROR = -1,
    PPR_MALLOC_ERROR = -2,
    PPR_PARSE_ERROR = -3
} param_process_t;

int param_process(char *filename,int style,
        int (*sfunc)(char *),
        int (*pfunc)(char *,char *));
char *getCurrentSection(void);

#ifdef __cplusplus
}
#endif

#endif  /* INI_H */
