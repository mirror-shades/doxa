#include <stdio.h>
#include <stdlib.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <time.h>
#endif

static long long monotonic_ns(void) {
#ifdef _WIN32
    static LARGE_INTEGER freq = {0};
    if (freq.QuadPart == 0) QueryPerformanceFrequency(&freq);
    LARGE_INTEGER c;
    QueryPerformanceCounter(&c);
    unsigned long long ticks = (unsigned long long)c.QuadPart;
    unsigned long long f = (unsigned long long)freq.QuadPart;
    return (long long)((ticks / f) * 1000000000ULL + (ticks % f) * 1000000000ULL / f);
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000000000LL + ts.tv_nsec;
#endif
}

int main(void) {
    const long long limit = 130000000;

    char* prime = malloc(limit + 1);

    /* Initialization (and first-touch of the buffer) happens before the timer,
       mirroring Doxa's array declaration so neither side pays page faults inside
       the timed region. */
    for (long long i = 0; i <= limit; i++) {
        prime[i] = 1;
    }

    long long t0 = monotonic_ns();

    prime[0] = 0;
    prime[1] = 0;

    for (long long i = 2; i * i <= limit; i++) {
        if (prime[i]) {
            for (long long j = i * i; j <= limit; j += i) {
                prime[j] = 0;
            }
        }
    }

    long long count = 0;
    for (long long i = 2; i <= limit; i++) {
        if (prime[i]) {
            count++;
        }
    }

    long long t1 = monotonic_ns();

    printf("%lld, %lld\n", t1 - t0, count);

    free(prime);
    return 0;
}
