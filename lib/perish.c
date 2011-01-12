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

    // Set a timer for half an hour
    alarm(1800);
}
