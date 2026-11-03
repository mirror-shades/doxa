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

/* Pop a string to empty, counting the bytes that are 'a'. The text cycles
   through "abc". */
int main(void) {
    const long long size = 10000;
    const long long rounds = 50;
    char *text = malloc((size_t)size);
    for (long long i = 0; i < size; i++) text[i] = (char)(97 + i % 3);
    long long checksum = 0;

    long long t0 = monotonic_ns();

    for (long long r = 0; r < rounds; r++) {
        long long len = size;
        while (len > 0) {
            char c = text[--len];
            if (c == 'a') checksum++;
        }
    }

    long long t1 = monotonic_ns();

    printf("%lld, %lld\n", t1 - t0, checksum);
    free(text);
    return 0;
}
