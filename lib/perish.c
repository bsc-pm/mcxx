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

#include <unistd.h>
#include <signal.h>
#include <string.h>

// This is a tiny library used for tests so they kill themselves after 30 min
// lest they hanged

static void terminating_signal_handler(int sig)
{
    // Kill ourselves
    raise(SIGKILL);
}

__attribute__((constructor)) static void perish_init(void)
{
    struct sigaction terminating_sigaction;
    memset(&terminating_sigaction, 0, sizeof(terminating_sigaction));

    terminating_sigaction.sa_handler = terminating_signal_handler;
    terminating_sigaction.sa_flags = SA_RESETHAND;
    // Block all blockable signals while handling the termination
    sigfillset(&terminating_sigaction.sa_mask);

    sigaction(SIGALRM, &terminating_sigaction, /* old_sigaction */ NULL);

    // Set a timer for 10 minutes
    alarm(600);
}
