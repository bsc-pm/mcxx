/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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


#define MCXX_REFCOUNT_DEBUG

#include <stdio.h>
#include <string.h>
#include <signal.h>
#include "mcxx_refcount.h"

/*
 * Implemented according to 
 *
 * "Concurrent Cycle Collection in Reference Counted Systems", David F. Bacon,
 * V.T. Rajan in Proceedings European Conference on OOP June 2001, LNCS, vol
 * 2072
 *
 * This file only implements de the synchronous collection
 *
 * Roger Ferrer Ibáñez - 2007
 *
 */

#define MCXX_REFCOUNT_ENTER_FUN(x)
#define MCXX_REFCOUNT_LEAVE_FUN(x)

#ifdef MCXX_REFCOUNT_DEBUG
 #ifdef MCXX_REFCOUNT_DEBUG_FUN
   #undef MCXX_REFCOUNT_ENTER_FUN
   #undef MCXX_REFCOUNT_LEAVE_FUN
   #define MCXX_REFCOUNT_ENTER_FUN(x) fprintf(stderr, "MCXX-REFCOUNT: entering -> %s(p=%p) \n",__FUNCTION__, (x))
   #define MCXX_REFCOUNT_LEAVE_FUN(x) fprintf(stderr, "MCXX-REFCOUNT: leaving <- %s(p=%p) \n",__FUNCTION__, (x))
 #endif
#endif

#ifdef __GNUC__
  #define UNUSED_PARAM __attribute__((unused))
#endif

static void _mcxx_nochildren(void *p, void (*_mcxx_do)(void*));

static void _mcxx_release(void *p);
static void _mcxx_possible_root(void *p);
static void _mcxx_append_to_roots(void *p);
static void _mcxx_remove_from_roots(void *p);
static void _mcxx_markroots(void);
static void _mcxx_scanroots(void);
static void _mcxx_collectroots(void);
static void _mcxx_markgray(void *p);
static void _mcxx_collectwhite(void *p);
static void _mcxx_scan(void *p);
static void _mcxx_scanblack(void *p);

#define MCXX_MAX_ROOTS (1024)
static int _mcxx_numroots = 0;
static void *_mcxx_roots[MCXX_MAX_ROOTS];

void *_mcxx_calloc(size_t nmemb, size_t size)
{
    void *p = calloc(nmemb, size);

#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Creating new object %p\n", __FUNCTION__, p);
#endif

    _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);
    
    // Set by default to nonchildren
    refp->_mcxx_children = _mcxx_nochildren;

    return p;
}

_mcxx_children_fun *_mcxx_children(void *p)
{
    _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);

	return &(refp->_mcxx_children);
}

static void _mcxx_nochildren(void *p UNUSED_PARAM, void (*_mcxx_do)(void*) UNUSED_PARAM)
{
    // Do nothing
}

void _mcxx_increment(void *p)
{
    if (p == NULL)
    {
        fprintf(stderr, "MCXX-REFCOUNT: %s: ERROR: Increment of reference on NULL pointer\n", __FUNCTION__);
        raise(SIGABRT);
    }
    MCXX_REFCOUNT_ENTER_FUN(p);
    _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);

#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Increasign refcount of %p from %d to %d\n", __FUNCTION__, p, 
            refp->_mcxx_refcount, refp->_mcxx_refcount+1);
#endif

    refp->_mcxx_refcount++;
    refp->_mcxx_colour = _MCXX_BLACK;
    MCXX_REFCOUNT_LEAVE_FUN(p);
}

void _mcxx_decrement(void *p)
{
    if (p != NULL)
    {
        MCXX_REFCOUNT_ENTER_FUN(p);
        _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);

#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Decrementing count of %p from %d to %d\n", __FUNCTION__, p,
                refp->_mcxx_refcount, refp->_mcxx_refcount-1);
#endif
        // Reduce the reference counter
        refp->_mcxx_refcount--;

        // If no references where hold
        if (refp->_mcxx_refcount == 0)
        {
            // Release it
#ifdef MCXX_REFCOUNT_DEBUG
            fprintf(stderr, "MCXX-REFCOUNT: %s Count of %p is zero, releasing it\n", __FUNCTION__, p);
#endif
            _mcxx_release(p);
        }
        else if (refp->_mcxx_refcount < 0)
        {
            fprintf(stderr, "MCXX-REFCOUNT: %s: ERROR: Count of %p is lower than zero\n", __FUNCTION__, p);
            raise(SIGABRT);
        }
        else
        {
            // Otherwise mark it as a possible root
#ifdef MCXX_REFCOUNT_DEBUG
            fprintf(stderr, "MCXX-REFCOUNT: %s Marking %p as a possible root (refcount=%d)\n", __FUNCTION__, p,
                    refp->_mcxx_refcount);
#endif
            _mcxx_possible_root(p);
        }
        MCXX_REFCOUNT_LEAVE_FUN(p);
    }
}

static void _mcxx_release(void *p)
{
    MCXX_REFCOUNT_ENTER_FUN(p);
    _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);

    // Recursively decrement all the childrens of refp
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Decrementing count of children of %p\n", __FUNCTION__, p);
#endif
    (refp->_mcxx_children)(refp, _mcxx_decrement);
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Children of %p decremented\n", __FUNCTION__, p);
#endif

#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Setting %p as black\n", __FUNCTION__, p);
#endif
    refp->_mcxx_colour = _MCXX_BLACK;

    // If not buffered free
    if (!refp->_mcxx_buffered)
    {
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Freeing non-buffered %p\n", __FUNCTION__, p);
#endif
        free(p);
    }
#ifdef MCXX_REFCOUNT_DEBUG
    else
    {
        fprintf(stderr, "MCXX-REFCOUNT: %s Not freeing %p since it is buffered\n", __FUNCTION__, p);
    }
#endif
    MCXX_REFCOUNT_LEAVE_FUN(p);
}

static void _mcxx_possible_root(void *p)
{
    MCXX_REFCOUNT_ENTER_FUN(p);
    _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);

    if (refp->_mcxx_colour != _MCXX_PURPLE)
    {
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Setting %p as purple\n", __FUNCTION__, p);
#endif
        refp->_mcxx_colour = _MCXX_PURPLE;
        if (!refp->_mcxx_buffered)
        {
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Setting %p as buffered and appending it to roots\n", __FUNCTION__, p);
#endif
            refp->_mcxx_buffered = 1;
            _mcxx_append_to_roots(p);
        }
        else
        {
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Setting %p was already buffered, not appending to roots\n", __FUNCTION__, p);
#endif
        }
    }
    MCXX_REFCOUNT_LEAVE_FUN(p);
}

void _mcxx_collectcycles(void)
{
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: *** Collecting cycles ***\n");
#endif
    _mcxx_markroots();
    _mcxx_scanroots();
    _mcxx_collectroots();
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: *** Ended collecting cycles ***\n");
#endif
}

static void _mcxx_markroots(void)
{
    int numroots = _mcxx_numroots;
    void *roots[MCXX_MAX_ROOTS];
    memcpy(roots, _mcxx_roots, sizeof(_mcxx_roots));

    int i;
    for (i = 0; i < numroots; i++)
    {
        void *p = roots[i];
        _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Considering object %p (%d of %d)\n", __FUNCTION__, p, i, numroots-1);
#endif

        if (refp->_mcxx_colour == _MCXX_PURPLE)
        {
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Object %p is purple, marking it gray\n", __FUNCTION__, p);
#endif
            _mcxx_markgray(p);
        }
        else
        {
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Object %p is not purple, marking not buffered and removing from roots\n", __FUNCTION__, p);
#endif
            refp->_mcxx_buffered = 0;
            _mcxx_remove_from_roots(p);
            if (refp->_mcxx_colour == _MCXX_BLACK
                    && refp->_mcxx_refcount == 0)
            {
#ifdef MCXX_REFCOUNT_DEBUG
                fprintf(stderr, "MCXX-REFCOUNT: %s Freeing %p because of it being black and refcount zero\n", __FUNCTION__, p);
#endif
                free(p);
            }

        }
    }
}

static void _mcxx_scanroots(void)
{
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Scanning roots\n", __FUNCTION__);
#endif
    int numroots = _mcxx_numroots;
    void *roots[MCXX_MAX_ROOTS];
    memcpy(roots, _mcxx_roots, sizeof(_mcxx_roots));

    int i;
    for (i = 0; i < numroots; i++)
    {
        void *p = roots[i];

        _mcxx_scan(p);
    }
}

static void _mcxx_collectroots(void)
{
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Collecting roots\n", __FUNCTION__);
#endif
    int numroots = _mcxx_numroots;
    void *roots[MCXX_MAX_ROOTS];
    memcpy(roots, _mcxx_roots, sizeof(_mcxx_roots));

    int i;
    for (i = 0; i < numroots; i++)
    {
        void *p = roots[i];
        _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);

#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Removing %p from roots and setting not buffered\n", __FUNCTION__, p);
#endif
        _mcxx_remove_from_roots(p);
        refp->_mcxx_buffered = 0;
        _mcxx_collectwhite(p);
    }
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Roots collected\n", __FUNCTION__);
#endif
}

// Auxiliar function for _mcxx_markgray
static void _mcxx_markgray_aux(void *p)
{
    if (p != NULL)
    {
        MCXX_REFCOUNT_ENTER_FUN(p);
        _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Decrementing count of %p from %d to %d\n", __FUNCTION__, p,
                refp->_mcxx_refcount, refp->_mcxx_refcount-1);
#endif
        refp->_mcxx_refcount--;
        _mcxx_markgray(p);
        MCXX_REFCOUNT_LEAVE_FUN(p);
    }
}

static void _mcxx_markgray(void *p)
{
    if (p != NULL)
    {
        MCXX_REFCOUNT_ENTER_FUN(p);
        _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);

        if (refp->_mcxx_colour != _MCXX_GRAY)
        {
#ifdef MCXX_REFCOUNT_DEBUG
            fprintf(stderr, "MCXX-REFCOUNT: %s Marking %p as gray\n", __FUNCTION__, p);
#endif
            refp->_mcxx_colour = _MCXX_GRAY;

            // For every children, reduce its refcount and markgray
            // recursively
            (refp->_mcxx_children)(p, _mcxx_markgray_aux);
        }
        MCXX_REFCOUNT_LEAVE_FUN(p);
    }
}

static void _mcxx_scan(void *p)
{
    if (p != NULL)
    {
        MCXX_REFCOUNT_ENTER_FUN(p);
        _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);

        if (refp->_mcxx_colour == _MCXX_GRAY)
        {
            if (refp->_mcxx_refcount > 0)
            {
#ifdef MCXX_REFCOUNT_DEBUG
                fprintf(stderr, "MCXX-REFCOUNT: %s Scanning blacks of %p since gray and refcount > 0\n", __FUNCTION__, p);
#endif
                _mcxx_scanblack(p);
            }
            else // refcount <= 0
            {
#ifdef MCXX_REFCOUNT_DEBUG
                fprintf(stderr, "MCXX-REFCOUNT: %s Setting %p as white and scanning children since it is gray and refcount <= 0\n", __FUNCTION__, p);
#endif
                refp->_mcxx_colour = _MCXX_WHITE;

                // For every children scan it
                (refp->_mcxx_children)(p, _mcxx_scan);
            }
        }
        MCXX_REFCOUNT_LEAVE_FUN(p);
    }
}

// Auxiliar function for _mcxx_scanblack
static void _mcxx_scanblack_aux(void *p)
{
    if (p != NULL)
    {
        MCXX_REFCOUNT_ENTER_FUN(p);
        _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);

#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Increasign refcount of %p from %d to %d\n", __FUNCTION__, p, 
                refp->_mcxx_refcount, refp->_mcxx_refcount+1);
#endif
        refp->_mcxx_refcount++;
        if (refp->_mcxx_colour != _MCXX_BLACK)
        {
            _mcxx_scanblack(p);
        }
        MCXX_REFCOUNT_LEAVE_FUN(p);
    }
}

static void _mcxx_scanblack(void *p)
{
    if (p != NULL)
    {
        MCXX_REFCOUNT_ENTER_FUN(p);
        _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);

#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Scanning black %p\n", __FUNCTION__, p);
#endif

#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Setting %p as black\n", __FUNCTION__, p);
#endif
        refp->_mcxx_colour = _MCXX_BLACK;

        // For every child reduce increase the reference counter and if not black
        // scan their blacks
        (refp->_mcxx_children)(p, _mcxx_scanblack_aux);
        MCXX_REFCOUNT_LEAVE_FUN(p);
    }
}

static void _mcxx_collectwhite(void *p)
{
    if (p != NULL)
    {
        MCXX_REFCOUNT_ENTER_FUN(p);
        _p_mcxx_base_refcount_t refp = (_p_mcxx_base_refcount_t)(p);

        if (refp->_mcxx_colour == _MCXX_WHITE
                && !refp->_mcxx_buffered)
        {
            refp->_mcxx_colour = _MCXX_BLACK;
            (refp->_mcxx_children)(p, _mcxx_collectwhite);

#ifdef MCXX_REFCOUNT_DEBUG
            fprintf(stderr, "MCXX-REFCOUNT: %s Freeing since white and not buffered %p\n", __FUNCTION__, p);
#endif
            free(p);
        }
        MCXX_REFCOUNT_LEAVE_FUN(p);
    }
}

static void _mcxx_append_to_roots(void *p)
{
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Adding %p to roots\n", __FUNCTION__, p);
#endif
    _mcxx_roots[_mcxx_numroots] = p;
    _mcxx_numroots++;

    // ??
    if (_mcxx_numroots == MCXX_MAX_ROOTS)
    {
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: Roots buffer full, collecting cycles\n");
#endif
        _mcxx_collectcycles();
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: After collecting cycles, roots buffer has %d objects\n", _mcxx_numroots);
#endif
    }
}

static void _mcxx_remove_from_roots(void *p)
{
    // First find in the list of roots
    int i;
    int index = -1;
    for (i = 0; i < _mcxx_numroots; i++)
    {
        if (_mcxx_roots[i] == p)
        {
            index = i;
            break;
        }
    }

    if (index < 0)
        return;

#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Removing %p from roots\n", __FUNCTION__, p);
#endif

    // Shift them
    for (i = (index + 1); i < _mcxx_numroots; i++)
    {
        _mcxx_roots[i-1] = _mcxx_roots[i];
    }
    
    // And decrease
    _mcxx_numroots--;
}

void mcxx_refcount_array_t_children(void *p, void (*f)(void*))
{
    mcxx_refcount_array_t* a = (mcxx_refcount_array_t*)p;
    int i;
    for (i = 0; i < a->num_items; i++)
    {
        f(a->data[i]);
    }
}

#if 0
static __attribute__((destructor)) void cleanup(void)
{
    _mcxx_collectcycles();
}
#endif
