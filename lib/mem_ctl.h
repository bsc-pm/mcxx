#ifndef MEM_CTL_H
#define MEM_CTL_H

#include <stdlib.h>

#define NEW(type)    			(type *) malloc(sizeof(type))
#define NEW0(type)				(type *) calloc(sizeof(type))
#define NEWSTR(str)				strdup((str))
#define FREESTR(str)			free((str))
#define NEW_ARRAY(type,size) 	(type *)malloc(sizeof(type)*size)
#define FREE(ptr) 				if (ptr) free(ptr)

#endif
