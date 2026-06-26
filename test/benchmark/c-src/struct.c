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

typedef struct {
    long long x, y, z, w;
} Vec4;

static long long sum_vec4(Vec4 v) {
    return v.x + v.y + v.z + v.w;
}

#define ITERS 1100000000LL

long long sum_vec4_loop(long long iters) {
    Vec4 v = {0, 1, 2, 3};
    long long sum = 0;
    long long i;

    for (i = 0; i < iters; i++) {
        v.x = i % 16;
        v.y = (i + 1) % 16;
        v.z = (i + 2) % 16;
        v.w = (i + 3) % 16;
        sum += sum_vec4(v);
    }
    return sum;
}

int main(void) {
    Vec4 v = {0, 1, 2, 3};
    long long sink = 0;
    long long i;

    for (i = 0; i < 110000000; i++) {
        v.x = i % 16;
        v.y = (i + 1) % 16;
        v.z = (i + 2) % 16;
        v.w = (i + 3) % 16;
        sink += sum_vec4(v);
    }

    long long t0 = monotonic_ns();
    long long result = sum_vec4_loop(ITERS);
    long long t1 = monotonic_ns();

    long long elapsed = t1 - t0;
    printf("%lld, %lld\n", elapsed, result + sink);
    return 0;
}
