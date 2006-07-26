#ifndef INI_H
#define INI_H

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <gc.h>

#define     MS_STYLE            1
#define     NOT_MS_STYLE        2

/*
** function prototypes
*/
int paramProcess(char *filename,int style,
		int (*sfunc)(char *),
		int (*pfunc)(char *,char *));
char *getCurrentSection(void);

#endif  /* INI_H */
