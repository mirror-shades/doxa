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

/* Line formatting: an int, a float (`i + 0.25`, whose text is `<i>.25`) and a
   string per line, each line its own allocation as on the Doxa side. */
int main(void) {
    const long long lines = 1000000;
    const char *names[4] = {"alpha", "beta", "gamma", "delta"};
    long long checksum = 0;

    long long t0 = monotonic_ns();

    for (long long i = 0; i < lines; i++) {
        int len = snprintf(NULL, 0, "%lld: %lld.25 %s|", i, i, names[i % 4]);
        char *line = malloc((size_t)len + 1);
        snprintf(line, (size_t)len + 1, "%lld: %lld.25 %s|", i, i, names[i % 4]);
        checksum += len;
        free(line);
    }

    long long t1 = monotonic_ns();

    printf("%lld, %lld\n", t1 - t0, checksum);
    return 0;
}
