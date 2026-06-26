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

#define N 250000
#define STEPS 550
#define MOD 65536LL

static Vec4 arr[N];

int main(void) {
    long long i, s;

    for (i = 0; i < N; i++) {
        arr[i].x = (i * 1) % MOD;
        arr[i].y = (i * 3 + 1) % MOD;
        arr[i].z = (i * 5 + 2) % MOD;
        arr[i].w = (i * 7 + 3) % MOD;
    }

    long long sink = 0;
    for (i = 0; i < N; i++)
        sink += arr[i].x + arr[i].y + arr[i].z + arr[i].w;

    long long t0 = monotonic_ns();
    long long carry = 1;
    for (s = 0; s < STEPS; s++) {
        for (i = 0; i < N; i++) {
            long long nx = (arr[i].x + carry) % MOD;
            long long ny = (arr[i].y + nx) % MOD;
            long long nz = (arr[i].z + ny) % MOD;
            long long nw = (arr[i].w + nz) % MOD;
            arr[i].x = nx;
            arr[i].y = ny;
            arr[i].z = nz;
            arr[i].w = nw;
            carry = (nw + 1) % MOD;
        }
    }
    long long t1 = monotonic_ns();

    long long checksum = 0;
    for (i = 0; i < N; i++)
        checksum += arr[i].x + arr[i].y + arr[i].z + arr[i].w;

    printf("%lld, %lld\n", t1 - t0, checksum + sink);
    return 0;
}
