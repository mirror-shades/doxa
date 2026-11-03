#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

/* `strbuild` ten times larger: loop-carried appends into a buffer that doubles
   when full. */
int main(void) {
    const long long rounds = 100;
    const long long appends = 50000;
    const char *pieces[4] = {"a", "bc", "def", "ghij"};
    const size_t lengths[4] = {1, 2, 3, 4};
    long long checksum = 0;

    long long t0 = monotonic_ns();

    for (long long r = 0; r < rounds; r++) {
        char *s = NULL;
        size_t len = 0;
        size_t cap = 0;
        for (long long i = 0; i < appends; i++) {
            const char *piece = pieces[(i + r) % 4];
            size_t piece_len = lengths[(i + r) % 4];
            if (len + piece_len > cap) {
                size_t grown = cap == 0 ? 8 : cap * 2;
                while (grown < len + piece_len) grown *= 2;
                s = realloc(s, grown);
                cap = grown;
            }
            memcpy(s + len, piece, piece_len);
            len += piece_len;
        }
        checksum += (long long)len;
        if (s[len / 2] == 'd') checksum += 1;
        free(s);
    }

    long long t1 = monotonic_ns();

    printf("%lld, %lld\n", t1 - t0, checksum);
    return 0;
}
