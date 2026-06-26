#include <stdio.h>
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

#define N     2000000
#define OUTER 600

int main(void) {
    static double x[N];
    static double y[N];
    long long i, r;

    for (i = 0; i < N; i++) {
        x[i] = (double)(i % 128);
        y[i] = 0.0;
    }

    double warm_total = 0.0;
    for (r = 0; r < 3; r++) {
        for (i = 0; i < N; i++)
            y[i] += 1.5 * x[i];

        double warm = 0.0;
        for (i = 0; i < N; i++)
            warm += y[i];
        warm_total += warm;

        for (i = 0; i < N; i++)
            y[i] = 0.0;
    }

    long long t0 = monotonic_ns();
    for (r = 0; r < OUTER; r++) {
        for (i = 0; i < N; i++)
            y[i] += 1.5 * x[i];
    }
    long long t1 = monotonic_ns();

    double checksum = 0.0;
    for (i = 0; i < N; i++)
        checksum += y[i];

    printf("%lld, %.1f\n", t1 - t0, checksum + warm_total);
    return 0;
}
