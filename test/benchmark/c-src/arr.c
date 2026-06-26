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

#define N         1000000
#define OUTER     7500

long long arr_reduce(long long *arr, long long n) {
    long long sum = 0;
    long long i, r;

    for (r = 0; r < OUTER; r++) {
        for (i = 0; i < N; i++) {
            sum += arr[i];
        }
    }
    return sum;
}

int main(void) {
    static long long arr[N];
    long long i;

    for (i = 0; i < N; i++)
        arr[i] = i % 997;

    long long sink = 0;
    for (i = 0; i < 75000000; i++)
        sink += arr[i % N];

    long long t0 = monotonic_ns();
    long long result = arr_reduce(arr, N);
    long long t1 = monotonic_ns();

    long long elapsed = t1 - t0;
    printf("%lld, %lld\n", elapsed, result + sink);
    return 0;
}
