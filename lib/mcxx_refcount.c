/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include "mem.h"

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

static void mcxx_release(void *p);
static void mcxx_possible_root(void *p);
static void mcxx_append_to_roots(void *p);
static void mcxx_remove_from_roots(void *p);
static void mcxx_markroots(void);
static void mcxx_scanroots(void);
static void mcxx_collectroots(void);
static void mcxx_markgray(void *p);
static void mcxx_collectwhite(void *p);
static void mcxx_scan(void *p);
static void mcxx_scanblack(void *p);

static void mcxx_add_to_free_set(void *p);
static void mcxx_release_free_set(void);

#define MCXX_MAX_ROOTS (1024)
static int mcxx_numroots = 0;
static void *mcxx_roots[MCXX_MAX_ROOTS];

#define GET_REFCOUNT_OBJ(p) \
    ({ p_mcxx_base_refcount_t q = \
          (p_mcxx_base_refcount_t)((char*)(p) - sizeof(mcxx_base_refcount_t)); \
      q; })

void mcxx_increment(void *p)
{
    if (p == NULL)
        return;

    MCXX_REFCOUNT_ENTER_FUN(p);
    p_mcxx_base_refcount_t refp = GET_REFCOUNT_OBJ(p);

#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Increasing refcount of %p from %d to %d\n", __FUNCTION__, p, 
            refp->count, refp->count+1);
#endif

    refp->count++;
    refp->colour = _MCXX_BLACK;
    MCXX_REFCOUNT_LEAVE_FUN(p);
}

void mcxx_decrement(void *p)
{
    if (p == NULL)
        return;

    MCXX_REFCOUNT_ENTER_FUN(p);
    p_mcxx_base_refcount_t refp = GET_REFCOUNT_OBJ(p);

#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Decrementing count of %p from %d to %d\n", __FUNCTION__, p,
            refp->count, refp->count-1);
#endif
    // Reduce the reference counter
    refp->count--;

    // If no references where hold
    if (refp->count == 0)
    {
        // Release it
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Count of %p is zero, releasing it\n", __FUNCTION__, p);
#endif
        mcxx_release(p);
    }
    else if (refp->count < 0)
    {
        fprintf(stderr, "MCXX-REFCOUNT: %s: ERROR: Count of %p is lower than zero\n", __FUNCTION__, p);
        raise(SIGABRT);
    }
    else
    {
        // Otherwise mark it as a possible root
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Marking %p as a possible root (refcount=%d)\n", __FUNCTION__, p,
                refp->count);
#endif
        mcxx_possible_root(p);
    }
    MCXX_REFCOUNT_LEAVE_FUN(p);

    mcxx_release_free_set();
}

static void mcxx_release(void *p)
{
    MCXX_REFCOUNT_ENTER_FUN(p);
    p_mcxx_base_refcount_t refp = GET_REFCOUNT_OBJ(p);

    // Recursively decrement all the children of refp
#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Decrementing count of children of %p\n", __FUNCTION__, p);
#endif
    if (refp->desc->walk != NULL)
        (refp->desc->walk)(p, mcxx_decrement);
#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Children of %p decremented\n", __FUNCTION__, p);
#endif

#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Setting %p as black\n", __FUNCTION__, p);
#endif
    refp->colour = _MCXX_BLACK;

    // If not buffered free
    if (!refp->buffered)
    {
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Adding non-buffered %p to free-set\n", __FUNCTION__, p);
#endif
        mcxx_add_to_free_set(p);
    }
#ifdef MCXX_REFCOUNT_DEBUG
    else
    {
        fprintf(stderr, "MCXX-REFCOUNT: %s Not freeing %p since it is buffered\n", __FUNCTION__, p);
    }
#endif
    MCXX_REFCOUNT_LEAVE_FUN(p);
}

static void mcxx_possible_root(void *p)
{
    MCXX_REFCOUNT_ENTER_FUN(p);
    p_mcxx_base_refcount_t refp = GET_REFCOUNT_OBJ(p);

    if (refp->colour != _MCXX_PURPLE)
    {
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Setting %p as purple\n", __FUNCTION__, p);
#endif
        refp->colour = _MCXX_PURPLE;
        if (!refp->buffered)
        {
#ifdef MCXX_REFCOUNT_DEBUG
            fprintf(stderr, "MCXX-REFCOUNT: %s Setting %p as buffered and appending it to roots\n", __FUNCTION__, p);
#endif
            refp->buffered = true;
            mcxx_append_to_roots(p);
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

void mcxx_collectcycles(void)
{
#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: *** Collecting cycles ***\n");
#endif
    mcxx_markroots();
    mcxx_scanroots();
    mcxx_collectroots();
    mcxx_release_free_set();
#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: *** Ended collecting cycles ***\n");
#endif
}

static void mcxx_markroots(void)
{
    // We need a copy of the roots because we will modify the roots while
    // traversing them. No other function does this
    int numroots = mcxx_numroots;
    void *roots[MCXX_MAX_ROOTS];
    memcpy(roots, mcxx_roots, sizeof(mcxx_roots));

    int i;
    for (i = 0; i < numroots; i++)
    {
        void *p = roots[i];
        p_mcxx_base_refcount_t refp = GET_REFCOUNT_OBJ(p);
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Considering object %p (%d of %d)\n", __FUNCTION__, p, i, numroots-1);
#endif

        if (refp->colour == _MCXX_PURPLE)
        {
#ifdef MCXX_REFCOUNT_DEBUG
            fprintf(stderr, "MCXX-REFCOUNT: %s Object %p is purple, marking it gray\n", __FUNCTION__, p);
#endif
            mcxx_markgray(p);
        }
        else
        {
#ifdef MCXX_REFCOUNT_DEBUG
            fprintf(stderr, "MCXX-REFCOUNT: %s Object %p is not purple, marking not buffered and removing from roots\n", __FUNCTION__, p);
#endif
            refp->buffered = false;
            mcxx_remove_from_roots(p);
            if (refp->colour == _MCXX_BLACK
                    && refp->count == 0)
            {
#ifdef MCXX_REFCOUNT_DEBUG
                fprintf(stderr, "MCXX-REFCOUNT: %s Adding %p to free set because of it being black and refcount zero\n",
                        __FUNCTION__, refp);
#endif
                mcxx_add_to_free_set(p);
            }
        }
    }
}

static void mcxx_scanroots(void)
{
#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Scanning roots\n", __FUNCTION__);
#endif

    int i;
    for (i = 0; i < mcxx_numroots; i++)
    {
        void *p = mcxx_roots[i];

        mcxx_scan(p);
    }
}

static void mcxx_collectroots(void)
{
#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Collecting roots\n", __FUNCTION__);
#endif
    int i;
    for (i = 0; i < mcxx_numroots; i++)
    {
        void *p = mcxx_roots[i];
        p_mcxx_base_refcount_t refp = GET_REFCOUNT_OBJ(p);

#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Removing %p from roots and setting not buffered\n", __FUNCTION__, p);
#endif
        refp->buffered = false;
        mcxx_collectwhite(p);
    }
    // we remove all objects from roots
    mcxx_numroots = 0;
#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Roots collected\n", __FUNCTION__);
#endif
}

// Auxiliar function for mcxx_markgray
static void mcxx_markgray_aux(void *p)
{
    if (p == NULL)
        return;

    MCXX_REFCOUNT_ENTER_FUN(p);
    p_mcxx_base_refcount_t refp = GET_REFCOUNT_OBJ(p);
#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Decrementing count of %p from %d to %d\n", __FUNCTION__, p,
            refp->count, refp->count-1);
#endif
    refp->count--;
    mcxx_markgray(p);
    MCXX_REFCOUNT_LEAVE_FUN(p);
}

static void mcxx_markgray(void *p)
{
    if (p == NULL)
        return;

    MCXX_REFCOUNT_ENTER_FUN(p);
    p_mcxx_base_refcount_t refp = GET_REFCOUNT_OBJ(p);

    if (refp->colour != _MCXX_GRAY)
    {
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Marking %p as gray\n", __FUNCTION__, p);
#endif
        refp->colour = _MCXX_GRAY;

        // For every children, reduce its refcount and markgray
        // recursively
        if (refp->desc->walk != NULL)
            (refp->desc->walk)(p, mcxx_markgray_aux);
    }
    MCXX_REFCOUNT_LEAVE_FUN(p);
}

static void mcxx_scan(void *p)
{
    if (p == NULL)
        return;

    MCXX_REFCOUNT_ENTER_FUN(p);
    p_mcxx_base_refcount_t refp = GET_REFCOUNT_OBJ(p);

    if (refp->colour == _MCXX_GRAY)
    {
        if (refp->count > 0)
        {
#ifdef MCXX_REFCOUNT_DEBUG
            fprintf(stderr, "MCXX-REFCOUNT: %s Scanning blacks of %p since gray and refcount > 0\n", __FUNCTION__, p);
#endif
            mcxx_scanblack(p);
        }
        else // refcount <= 0
        {
#ifdef MCXX_REFCOUNT_DEBUG
            fprintf(stderr, "MCXX-REFCOUNT: %s Setting %p as white and scanning children since it is gray and refcount <= 0\n", __FUNCTION__, p);
#endif
            refp->colour = _MCXX_WHITE;

            // For every children scan it
            if (refp->desc->walk != NULL)
                (refp->desc->walk)(p, mcxx_scan);
        }
    }
    MCXX_REFCOUNT_LEAVE_FUN(p);
}

// Auxiliar function for mcxx_scanblack
static void mcxx_scanblack_aux(void *p)
{
    if (p == NULL)
        return;

    MCXX_REFCOUNT_ENTER_FUN(p);
    p_mcxx_base_refcount_t refp = GET_REFCOUNT_OBJ(p);

#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Increasing refcount of %p from %d to %d\n", __FUNCTION__, p, 
            refp->count, refp->count+1);
#endif
    refp->count++;
    if (refp->colour != _MCXX_BLACK)
    {
        mcxx_scanblack(p);
    }
    MCXX_REFCOUNT_LEAVE_FUN(p);
}

static void mcxx_scanblack(void *p)
{
    if (p == NULL)
        return;

    MCXX_REFCOUNT_ENTER_FUN(p);
    p_mcxx_base_refcount_t refp = GET_REFCOUNT_OBJ(p);

#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Scanning black %p\n", __FUNCTION__, p);
#endif

#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Setting %p as black\n", __FUNCTION__, p);
#endif
    refp->colour = _MCXX_BLACK;

    // For every child reduce increase the reference counter and if not black
    // scan their blacks
    if (refp->desc->walk != NULL)
        (refp->desc->walk)(p, mcxx_scanblack_aux);
    MCXX_REFCOUNT_LEAVE_FUN(p);
}

static void mcxx_collectwhite(void *p)
{
    if (p == NULL)
        return;

    MCXX_REFCOUNT_ENTER_FUN(p);
    p_mcxx_base_refcount_t refp = GET_REFCOUNT_OBJ(p);

    if (refp->colour == _MCXX_WHITE
            && !refp->buffered)
    {
        refp->colour = _MCXX_BLACK;
        if (refp->desc->walk != NULL)
            (refp->desc->walk)(p, mcxx_collectwhite);

#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Adding %p to free set since white and not buffered\n", __FUNCTION__, p);
#endif
        mcxx_add_to_free_set(p);
    }
    MCXX_REFCOUNT_LEAVE_FUN(p);
}

static void mcxx_append_to_roots(void *p)
{
#ifdef MCXX_REFCOUNT_DEBUG
    fprintf(stderr, "MCXX-REFCOUNT: %s Adding %p to roots\n", __FUNCTION__, p);
#endif
    mcxx_roots[mcxx_numroots] = p;
    mcxx_numroots++;

    // ??
    if (mcxx_numroots == MCXX_MAX_ROOTS)
    {
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: Roots buffer full, collecting cycles\n");
#endif
        mcxx_collectcycles();
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: After collecting cycles, roots buffer has %d objects\n", mcxx_numroots);
#endif
    }
}

static void mcxx_remove_from_roots(void *p)
{
    // First find in the list of roots
    int i;
    int index = -1;
    for (i = 0; i < mcxx_numroots; i++)
    {
        if (mcxx_roots[i] == p)
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
    for (i = (index + 1); i < mcxx_numroots; i++)
    {
        mcxx_roots[i-1] = mcxx_roots[i];
    }

    // And decrease
    mcxx_numroots--;
}

#if 0
static __attribute__((destructor)) void cleanup(void)
{
    mcxx_collectcycles();
}
#endif

#if 0
#endif

static int free_set_capacity = 0;
static int free_set_size = 0;
static void **free_set = NULL;

static void mcxx_add_to_free_set(void *p)
{
    char found = 0;
    int i = 0;
    // We could make this more efficient
    for (i = 0; i < free_set_size && !found; i++)
    {
        if (free_set[i] == p)
            found = 1;
    }

    if (found)
        return;

    if (free_set_size == free_set_capacity)
    {
        free_set_capacity = free_set_capacity*2 + 1;
        free_set = NEW_REALLOC(void*, free_set, free_set_capacity);
    }

    free_set[free_set_size] = p;
    free_set_size++;
}

static void mcxx_release_free_set(void)
{
    int i;
    for (i = 0; i < free_set_size; i++)
    {
        void *p = free_set[i];
        p_mcxx_base_refcount_t refp = GET_REFCOUNT_OBJ(p);
        if (refp->desc->dealloc != NULL)
        {
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Calling dealloc function for %p\n", __FUNCTION__, p);
#endif
            (refp->desc->dealloc)(p);
        }
#ifdef MCXX_REFCOUNT_DEBUG
        fprintf(stderr, "MCXX-REFCOUNT: %s Freeing descriptor of %p\n", __FUNCTION__, p);
#endif
        xfree(refp);
    }

    free_set_size = 0;

    // Not sure if we want to do this every time
    free_set_capacity = 0;
    xfree(free_set); free_set = NULL;
}
