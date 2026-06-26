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
    return (long long)((unsigned long long)c.QuadPart * 1000000000ULL /
                       (unsigned long long)freq.QuadPart);
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000000000LL + ts.tv_nsec;
#endif
}

static long long leaf_add(long long a, long long b) {
    return a + b;
}

#define ITERS 350000000LL

long long leaf_sum(long long iters) {
    long long sum = 0;
    long long i;

    for (i = 0; i < iters; i++) {
        long long a = leaf_add(i, (sum % 997) + 1);
        sum += a;
    }
    return sum;
}

int main(void) {
    long long sink = 0;
    long long i;

    for (i = 0; i < 35000000; i++)
        sink += leaf_add(i, (sink % 997) + 1);

    long long t0 = monotonic_ns();
    long long result = leaf_sum(ITERS);
    long long t1 = monotonic_ns();

    long long elapsed = t1 - t0;
    printf("%lld, %lld\n", elapsed, result + sink);
    return 0;
}
